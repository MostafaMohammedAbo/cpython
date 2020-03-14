/*
** Copyright (c) 2000 D. Richard Hipp
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public
** License as published by the Free Software Foundation; either
** version 2 of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** General Public License for more details.
**
** You should have received a copy of the GNU General Public
** License along with this library; if not, write to the
** Free Software Foundation, Inc., 59 Temple Place - Suite 330,
** Boston, MA  02111-1307, USA.
**
** Author contact information:
**   drh@hwaci.com
**   http://www.hwaci.com/drh/
**
*************************************************************************
** A ZIP archive virtual filesystem for Tcl.
**
** This package of routines enables Tcl to use a Zip file as
** a virtual file system.  Each of the content files of the Zip
** archive appears as a real file to Tcl.
**
** Modified to use Tcl VFS hooks by Peter MacDonald
**   peter@pdqi.com
**   http://pdqi.com
**
**   Revison  Date           Author             Description
**   -------  -------------  -----------------  ----------------------------------------------
**            Jan  8, 2006   Dennis R. LaBelle  Modified to support encrypted files
**
**            Dec 16, 2009   Dennis R. LaBelle  Corrected Tobe_FSMatchInDirectoryProc() for
**                                              proper operation of glob command on ZVFS files
**                                              under TCL 8.5.
**
**            Jan 10, 2012   Chris R. Stewart   Corrected Tobe_FSFileAttrStringsProc() pointer type.
**                                              Corrected ZvfsFileOpen() for proper pointer data type.
**                                              Added inttypes.h for use of arch independant pointer sized integers.
**            2020-03-14     xxx                Support zip file with prepended data.
**                                              Fix many error handling.
*/

#include <ctype.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <stdlib.h>
#include <malloc.h>
#include <inttypes.h>
#include <zlib.h>
#include <tcl.h>

/*
** Size of the decompression input buffer
*/
#define COMPR_BUF_SIZE   32768
static int maptolower = 0;

/*
** All static variables are collected into a structure named "local".
** That way, it is clear in the code when we are using a static
** variable because its name begins with "local.".
*/
static struct {
  Tcl_HashTable fileHash;     /* One entry for each file in the ZVFS.  The
                              ** The key is the virtual filename.  The data
                              ** an an instance of the ZvfsFile structure. */
  Tcl_HashTable archiveHash;  /* One entry for each archive.  Key is the name.
                              ** data is the ZvfsArchive structure */
  int isInit;                 /* True after initialization */
} local;

/*
** Each ZIP archive file that is mounted is recorded as an instance
** of this structure
*/
typedef struct ZvfsArchive {
  char *zName;              /* Name of the archive */
  char *zMountPoint;        /* Where this archive is mounted */
  struct ZvfsFile *pFiles;  /* List of files in that archive */
} ZvfsArchive;

/*
** Particulars about each virtual file are recorded in an instance
** of the following structure.
*/
typedef struct ZvfsFile {
  char *zName;              /* The full pathname of the virtual file */
  ZvfsArchive *pArchive;    /* The ZIP archive holding this file data */
  int iOffset;              /* Offset into the ZIP archive of the data */
  int nByte;                /* Uncompressed size of the virtual file */
  int nByteCompr;           /* Compressed size of the virtual file */
  int isdir;                /* Set to 1 if directory */
  int depth;                /* Number of slashes in path. */
  int timestamp;            /* Modification time */
  struct ZvfsFile *pNext;      /* Next file in the same archive */
  struct ZvfsFile *pNextName;  /* A doubly-linked list of files with the same */
  struct ZvfsFile *pPrevName;  /*  name.  Only the first is in local.fileHash */
  /* The following would be used for writable zips.                           */
  int nExtra;               /* Extra space in the TOC header */
  int isSpecial;            /* Not really a file in the ZIP archive */
  int dosTime;              /* Modification time (DOS format) */
  int dosDate;              /* Modification date (DOS format) */
  int iCRC;                 /* Cyclic Redundancy Check of the data */
} ZvfsFile;


/*
** Macros to read 16-bit and 32-bit big-endian integers into the
** native format of this local processor.  B is an array of
** characters and the integer begins at the N-th character of
** the array.
*/
#define INT16(B, N) (B[N] + (B[N+1]<<8))
#define INT32(B, N) (INT16(B,N) + (B[N+2]<<16) + (B[N+3]<<24))

/*
** Write a 16- or 32-bit integer as little-endian into the given buffer.
*/
static void put16(char *z, int v){
  z[0] = v & 0xff;
  z[1] = (v>>8) & 0xff;
}
static void put32(char *z, int v){
  z[0] = v & 0xff;
  z[1] = (v>>8) & 0xff;
  z[2] = (v>>16) & 0xff;
  z[3] = (v>>24) & 0xff;
}

/* Convert DOS time to unix time. */
static time_t DosTimeDate(int dosDate, int dosTime){
  time_t now;
  struct tm *tm;
  now=time(NULL);
  tm = localtime(&now);
  tm->tm_isdst = -1;
  tm->tm_year=(((dosDate&0xfe00)>>9) + 80);
  tm->tm_mon=((dosDate&0x1e0)>>5)-1;
  tm->tm_mday=(dosDate & 0x1f);
  tm->tm_hour=(dosTime&0xf800)>>11;
  tm->tm_min=(dosTime&0x7e0)>>5;
  tm->tm_sec=(dosTime&0x1f) * 2;
  return mktime(tm);
}

/* Return count of char ch in str */
int strchrcnt(const char *str, char ch) {
  int cnt = 0;
  for (const char *cp = str; (cp = strchr(cp, ch)); cp++) {
    cnt++;
  }
  return cnt;
}


/*
** Concatenate zTail onto zRoot to form a pathname.  zRoot will begin
** with "/".  After concatenation, simplify the pathname by removing
** unnecessary ".." and "." directories.  Under windows, make all
** characters lower case.
**
** Resulting pathname is returned.  Space to hold the returned path is
** obtained from Tcl_Alloc() and should be freed by the calling function.
*/
static char *CanonicalPath(const char *zRoot, const char *zTail){
  char *zPath;
  int i, j, c;

#ifdef __WIN32__
  if( isalpha(zTail[0]) && zTail[1]==':' ){ zTail += 2; }
  if( zTail[0]=='\\' ){ zRoot = ""; zTail++; }
  if( zTail[0]=='\\' ){ zRoot = "/"; zTail++; }   // account for UNC style path
#endif
  if( zTail[0]=='/' ){ zRoot = ""; zTail++; }
  if( zTail[0]=='/' ){ zRoot = "/"; zTail++; }    // account for UNC style path
  zPath = Tcl_Alloc((unsigned int)(strlen(zRoot) + strlen(zTail) + 2));
  if( zPath==0 ) return 0;
  sprintf(zPath, "%s/%s", zRoot, zTail);
  for(i=j=0; (c = zPath[i])!=0; i++){
#ifdef __WIN32__
    if( isupper(c) ) { if (maptolower) c = tolower(c); }
    else if( c=='\\' ) c = '/';
#endif
    if( c=='/' ){
      int c2 = zPath[i+1];
      if( c2=='/' ) continue;
      if( c2=='.' ){
        int c3 = zPath[i+2];
        if( c3=='/' || c3==0 ){
          i++;
          continue;
        }
        if( c3=='.' && (zPath[i+3]=='.' || zPath[i+3]==0) ){
          i += 2;
          while( j>0 && zPath[j-1]!='/' ){ j--; }
          continue;
        }
      }
    }
    zPath[j++] = c;
  }
  if( j==0 ){ zPath[j++] = '/'; }
  zPath[j] = 0;
  return zPath;
}

/*
** Construct an absolute pathname in memory obtained from Tcl_Alloc
** that means the same file as the pathname given.
**
** Under windows, all backslash (\) charaters are converted to foward
** slash (/) and all upper case letters are converted to lower case.
** The drive letter (if present) is preserved.
*/
static char *AbsolutePath(const char *z) {
  Tcl_DString pwd;
  char *zResult;
  Tcl_DStringInit(&pwd);
  if( *z!='/'
#ifdef __WIN32__
    && *z!='\\' && (!isalpha(*z) || z[1]!=':')
#endif
  ){
    /* Case 1:  "z" is a relative path.  So prepend the current working
    ** directory in order to generate an absolute path.  Note that the
    ** CanonicalPath() function takes care of converting upper to lower
    ** case and (\) to (/) under windows.
    */
    Tcl_GetCwd(0, &pwd);
    zResult = CanonicalPath( Tcl_DStringValue(&pwd), z);
    Tcl_DStringFree(&pwd);
  } else {
    /* Case 2:  "z" is an absolute path already.  We just need to make
    ** a copy of it.  Under windows, we need to convert upper to lower
    ** case and (\) into (/) on the copy.
    */
    zResult = Tcl_Alloc((unsigned int)strlen(z) + 1);
    strcpy(zResult, z);
#ifdef __WIN32__
    {
      int i, c;
      for(i=0; (c=zResult[i])!=0; i++){
        if( isupper(c) ) {
          // zResult[i] = tolower(c);
        }
        else if( c=='\\' ) zResult[i] = '/';
      }
    }
#endif
  }
  return zResult;
}

static void ZvfsFreeArchive(ZvfsArchive *pArchive) {
  for (ZvfsFile *pZvfs = pArchive->pFiles; pZvfs != NULL; pZvfs = pZvfs->pNext) {
    Tcl_HashEntry *pFileEntry = Tcl_FindHashEntry(&local.fileHash, pZvfs->zName);
    if (pFileEntry) {
      Tcl_DeleteHashEntry(pFileEntry);
    }
    Tcl_Free(pZvfs->zName);
  }
  Tcl_Free(pArchive->zName);
  Tcl_Free((char *)pArchive);
}

/*
** Read a ZIP archive and make entries in the virutal file hash table for all
** content files of that ZIP archive.  Also initialize the ZVFS if this
** routine has not been previously called.
*/
int Zvfs_Mount(
  Tcl_Interp *interp,          /* Leave error messages in this interpreter */
  const char *zArchive,        /* The ZIP archive file */
  const char *zMountPoint      /* Mount contents at this directory */
) {
  int return_code = TCL_OK;
  Tcl_Channel chan = NULL;            /* Used for reading the ZIP archive file */
  char *zArchiveName = NULL;          /* A copy of zArchive */
  int32_t iEOCD = 0;                  /* Position of the End of central directory record */
  ZvfsArchive *pArchive = NULL;       /* The ZIP archive being mounted */
  Tcl_HashEntry *pEntry = NULL;       /* Hash table entry */
  int isNew = 0;                      /* Flag to tell use when a hash entry is new */
  unsigned char zBuf[100];            /* Space into which to read from the ZIP archive */
  Tcl_HashSearch zSearch;             /* Search all mount points */
  int32_t iArchiveOffset = 0;         /* Start of archive */

#define RETURN_ERR return_code = TCL_ERROR; goto L_RETURN

  if (!local.isInit) return TCL_ERROR;
  if (!zArchive) {
    pEntry = Tcl_FirstHashEntry(&local.archiveHash, &zSearch);
    while (pEntry) {
      if ((pArchive = Tcl_GetHashValue(pEntry))) {
        Tcl_AppendResult(interp, pArchive->zMountPoint, " ", pArchive->zName, " ", 0);
      }
      pEntry = Tcl_NextHashEntry(&zSearch);
    }
    return TCL_OK;
  }
  if (!zMountPoint) {
    zArchiveName = AbsolutePath(zArchive);
    pEntry = Tcl_FindHashEntry(&local.archiveHash, zArchiveName);
    if (pEntry) {
      if ((pArchive = Tcl_GetHashValue(pEntry))) {
        Tcl_AppendResult(interp, pArchive->zMountPoint, 0);
      }
    }
    Tcl_Free(zArchiveName);
    return TCL_OK;
  }

  chan = Tcl_OpenFileChannel(interp, zArchive, "r", 0);
  if (!chan) {
    RETURN_ERR;
  }
  if (Tcl_SetChannelOption(interp, chan, "-translation", "binary") != TCL_OK){
    RETURN_ERR;
  }
  if (Tcl_SetChannelOption(interp, chan, "-encoding", "binary") != TCL_OK) {
    RETURN_ERR;
  }

  /* Read the "End Of Central Directory" record from the end of the
  ** ZIP archive.
  */
  iEOCD = (int32_t)Tcl_Seek(chan, -22, SEEK_END);
  if (22 != Tcl_Read(chan, zBuf, 22) || 0 != memcmp(zBuf, "\120\113\05\06", 4)) {
    Tcl_AppendResult(interp, "not a ZIP archive", NULL);
    RETURN_ERR;
  }

  /* Construct the archive record
  */
  zArchiveName = AbsolutePath(zArchive);
  pEntry = Tcl_CreateHashEntry(&local.archiveHash, zArchiveName, &isNew);
  if (!isNew) {
    pArchive = Tcl_GetHashValue(pEntry);
    Tcl_AppendResult(interp, "already mounted at ", pArchive->zMountPoint, 0);
    pEntry = NULL;
    pArchive = NULL;
    RETURN_ERR;
  }
  pArchive = (ZvfsArchive *)Tcl_Alloc(sizeof(*pArchive) + (unsigned int)strlen(zMountPoint) + 1);
  pArchive->zName = zArchiveName;
  zArchiveName = NULL;
  pArchive->zMountPoint = (char *)&pArchive[1];
  strcpy(pArchive->zMountPoint, zMountPoint);
  pArchive->pFiles = NULL;
  Tcl_SetHashValue(pEntry, pArchive);

  /* Compute the starting location of the directory for the ZIP archive
  ** then seek to that location.
  */
  {
    int32_t header_size = INT32(zBuf, 12);    // Size of central directory (bytes)
    int32_t header_offset = INT32(zBuf, 16);  // Offset of start of central directory, relative to start of archive
    iArchiveOffset = iEOCD - header_size - header_offset;  // Start of archive
    if (iArchiveOffset < 0) {
      Tcl_AppendResult(interp, "ill-formed central directory entry, iArchiveOffset < 0.", NULL);
      RETURN_ERR;
    }
    Tcl_Seek(chan, iEOCD - header_size, SEEK_SET);
  }

  for (int nFile = INT16(zBuf, 8); nFile > 0; nFile--) {
    char zName[1024];       /* Space to hold the filename */

    /* Read the next directory entry.  Extract the size of the filename,
    ** the size of the "extra" information, and the offset into the archive
    ** file of the file data.
    */
    if (46 != Tcl_Read(chan, zBuf, 46) || 0 != memcmp(zBuf, "\120\113\01\02", 4)) {
      Tcl_AppendResult(interp, "ill-formed central directory entry", NULL);
      RETURN_ERR;
    }
    int lenName = INT16(zBuf, 28);  /* Length of the next filename */
    int lenExtra = INT16(zBuf, 30) + INT16(zBuf, 32); /* Length of "extra" data for next file */
    int iData = INT32(zBuf, 42);    /* Offset to start of file data */

    /* If the virtual filename is too big to fit in zName[], then skip
    ** this file
    */
    if (lenName >= sizeof(zName)) {
      Tcl_Seek(chan, lenName + lenExtra, SEEK_CUR);
      continue;
    }

    /* Construct an entry in local.fileHash for this virtual file.
    */
    if (lenName != Tcl_Read(chan, zName, lenName)) {
      Tcl_AppendResult(interp, "error read file name in central directory entry", NULL);
      RETURN_ERR;
    }

    int isdir = 0;
    if (lenName > 0 && zName[lenName - 1] == '/') {
      lenName--;
      isdir = 1;
    }

    zName[lenName] = 0;
    ZvfsFile *pZvfs = (ZvfsFile *)Tcl_Alloc(sizeof(*pZvfs));
    pZvfs->zName = CanonicalPath(zMountPoint, zName);
    pZvfs->pArchive = pArchive;
    pZvfs->isdir = isdir;
    pZvfs->depth = strchrcnt(pZvfs->zName, '/');
    pZvfs->iOffset = iArchiveOffset + iData;  // Adjust offset
    int dosDate = INT16(zBuf, 14);
    int dosTime = INT16(zBuf, 12);
    pZvfs->timestamp = (int)DosTimeDate(dosDate, dosTime);
    pZvfs->nByte = INT32(zBuf, 24);
    pZvfs->nByteCompr = INT32(zBuf, 20);
    pZvfs->pNext = pArchive->pFiles;
    pArchive->pFiles = pZvfs;
    Tcl_HashEntry *pFileEntry = Tcl_CreateHashEntry(&local.fileHash, pZvfs->zName, &isNew);
    if (isNew) {
      pZvfs->pNextName = 0;
    } else {
      ZvfsFile *pOld = (ZvfsFile *)Tcl_GetHashValue(pFileEntry);
      pOld->pPrevName = pZvfs;
      pZvfs->pNextName = pOld;
    }
    pZvfs->pPrevName = 0;
    Tcl_SetHashValue(pFileEntry, (ClientData)pZvfs);

    /* Skip over the extra information so that the next read will be from
    ** the beginning of the next directory entry.
    */
    Tcl_Seek(chan, lenExtra, SEEK_CUR);
  }
  // OK
  pEntry = NULL;

L_RETURN:
  if (zArchiveName) {
    Tcl_Free(zArchiveName);
  }
  if (chan) {
    Tcl_Close(interp, chan);
  }
  if (pEntry) {
    Tcl_DeleteHashEntry(pEntry);
    ZvfsFreeArchive(pArchive);
  }
  return return_code;
}

/*
** Locate the ZvfsFile structure that corresponds to the file named.
** Return NULL if there is no such ZvfsFile.
*/
static ZvfsFile *ZvfsLookup(const char *zFilename) {
  if (!local.isInit) {
    return NULL;
  }
  char *zTrueName = AbsolutePath(zFilename);
  Tcl_HashEntry *pEntry = Tcl_FindHashEntry(&local.fileHash, zTrueName);
  Tcl_Free(zTrueName);
  ZvfsFile *pFile = pEntry ? Tcl_GetHashValue(pEntry) : NULL;
  return pFile;
}

static int ZvfsLookupMount(const char *zFilename){
  if (!local.isInit) {
    return 0;
  }

  int match = 0;
  Tcl_HashSearch zSearch;   /* Search all mount points */
  char *zTrueName = AbsolutePath(zFilename);
  Tcl_HashEntry *pEntry = Tcl_FirstHashEntry(&local.archiveHash, &zSearch);
  while (pEntry) {
    ZvfsArchive *pArchive = Tcl_GetHashValue(pEntry);
    if (pArchive && 0 == strcmp(pArchive->zMountPoint, zTrueName)) {
      match = 1;
      break;
    }
    pEntry = Tcl_NextHashEntry(&zSearch);
  }
  Tcl_Free(zTrueName);
  return match;
}


/*
** Unmount all the files in the given ZIP archive.
*/
static void Zvfs_Unmount(CONST char *zArchive){
  char *zArchiveName = AbsolutePath(zArchive);
  Tcl_HashEntry *pEntry = Tcl_FindHashEntry(&local.archiveHash, zArchiveName);
  Tcl_Free(zArchiveName);
  if (!pEntry) {
    return;
  }
  ZvfsArchive *pArchive = Tcl_GetHashValue(pEntry);
  Tcl_DeleteHashEntry(pEntry);
  ZvfsFreeArchive(pArchive);
}

/*
** zvfs::mount  Zip-archive-name  mount-point
**
** Create a new mount point on the given ZIP archive.  After this
** command executes, files contained in the ZIP archive will appear
** to Tcl to be regular files at the mount point.
*/
static int ZvfsMountCmd(
  ClientData clientData,             /* Client data for this command */
  Tcl_Interp *interp,        /* The interpreter used to report errors */
  int argc,                  /* Number of arguments */
  const char *argv[]                /* Values of all arguments */
) {
  if (argc > 3) {
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
       " ? ZIP-FILE ? MOUNT-POINT ? ?\"", 0);
    return TCL_ERROR;
  }
  return Zvfs_Mount(interp, argc > 1 ? argv[1] : 0, argc > 2 ? argv[2] : 0);
}

/*
** zvfs::unmount  Zip-archive-name
**
** Undo the effects of zvfs::mount.
*/
static int ZvfsUnmountCmd(
  ClientData clientData,             /* Client data for this command */
  Tcl_Interp *interp,        /* The interpreter used to report errors */
  int argc,                  /* Number of arguments */
  const char *argv[]                /* Values of all arguments */
) {
  if (argc != 2) {
    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
       " ZIP-FILE\"", 0);
    return TCL_ERROR;
  }
  Zvfs_Unmount(argv[1]);
  return TCL_OK;
}

/*
** zvfs::exists  filename
**
** Return TRUE if the given filename exists in the ZVFS and FALSE if
** it does not.
*/
static int ZvfsExistsObjCmd(
  void *NotUsed,             /* Client data for this command */
  Tcl_Interp *interp,        /* The interpreter used to report errors */
  int objc,                  /* Number of arguments */
  Tcl_Obj *const* objv       /* Values of all arguments */
) {
  if (objc != 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "FILENAME");
    return TCL_ERROR;
  }
  char *zFilename = Tcl_GetStringFromObj(objv[1], 0);
  Tcl_SetBooleanObj(Tcl_GetObjResult(interp), ZvfsLookup(zFilename) != 0);
  return TCL_OK;
}


/*
** zvfs::info  filename
**
** Return information about the given file in the ZVFS.  The information
** consists of (1) the name of the ZIP archive that contains the file,
** (2) the size of the file after decompressions, (3) the compressed
** size of the file, and (4) the offset of the compressed data in the archive.
*/
static int ZvfsInfoObjCmd(
  void *NotUsed,             /* Client data for this command */
  Tcl_Interp *interp,        /* The interpreter used to report errors */
  int objc,                  /* Number of arguments */
  Tcl_Obj *const* objv       /* Values of all arguments */
) {
  if (objc != 2) {
    Tcl_WrongNumArgs(interp, 1, objv, "FILENAME");
    return TCL_ERROR;
  }
  char *zFilename = Tcl_GetStringFromObj(objv[1], 0);
  ZvfsFile *pFile = ZvfsLookup(zFilename);
  if (pFile) {
    Tcl_Obj *pResult = Tcl_GetObjResult(interp);
    Tcl_ListObjAppendElement(interp, pResult,
       Tcl_NewStringObj(pFile->pArchive->zName, -1));
    Tcl_ListObjAppendElement(interp, pResult, Tcl_NewIntObj(pFile->nByte));
    Tcl_ListObjAppendElement(interp, pResult, Tcl_NewIntObj(pFile->nByteCompr));
    Tcl_ListObjAppendElement(interp, pResult, Tcl_NewIntObj(pFile->iOffset));
  }
  return TCL_OK;
}


/*
** zvfs::list
**
** Return a list of all files in the ZVFS.  The order of the names
** in the list is arbitrary.
*/
static int ZvfsListObjCmd(
  void *NotUsed,             /* Client data for this command */
  Tcl_Interp *interp,        /* The interpreter used to report errors */
  int objc,                  /* Number of arguments */
  Tcl_Obj *const* objv       /* Values of all arguments */
) {
  char *zPattern = 0;
  Tcl_RegExp pRegexp = 0;
  Tcl_HashEntry *pEntry;
  Tcl_HashSearch sSearch;
  Tcl_Obj *pResult = Tcl_GetObjResult(interp);

  if( objc>3 ){
    Tcl_WrongNumArgs(interp, 1, objv, "?(-glob|-regexp)? ?PATTERN?");
    return TCL_ERROR;
  }
  if( local.isInit==0 ) return TCL_OK;
  if( objc==3 ){
    int n;
    char *zSwitch = Tcl_GetStringFromObj(objv[1], &n);
    if( n>=2 && strncmp(zSwitch,"-glob",n)==0 ){
      zPattern = Tcl_GetString(objv[2]);
    }else if( n>=2 && strncmp(zSwitch,"-regexp",n)==0 ){
      pRegexp = Tcl_RegExpCompile(interp, Tcl_GetString(objv[2]));
      if( pRegexp==0 ) return TCL_ERROR;
    }else{
      Tcl_AppendResult(interp, "unknown option: ", zSwitch, 0);
      return TCL_ERROR;
    }
  }else if( objc==2 ){
    zPattern = Tcl_GetStringFromObj(objv[1], 0);
  }
  if( zPattern ){
    for(pEntry = Tcl_FirstHashEntry(&local.fileHash, &sSearch);
        pEntry;
        pEntry = Tcl_NextHashEntry(&sSearch)
    ){
      ZvfsFile *pFile = (ZvfsFile *)Tcl_GetHashValue(pEntry);
      char *z = pFile->zName;
      if( Tcl_StringMatch(z, zPattern) ){
        Tcl_ListObjAppendElement(interp, pResult, Tcl_NewStringObj(z, -1));
      }
    }
  }else if( pRegexp ){
    for(pEntry = Tcl_FirstHashEntry(&local.fileHash, &sSearch);
        pEntry;
        pEntry = Tcl_NextHashEntry(&sSearch)
    ){
      ZvfsFile *pFile = (ZvfsFile *)Tcl_GetHashValue(pEntry);
      char *z = pFile->zName;
      if( Tcl_RegExpExec(interp, pRegexp, z, z) ){
        Tcl_ListObjAppendElement(interp, pResult, Tcl_NewStringObj(z, -1));
      }
    }
  }else{
    for(pEntry = Tcl_FirstHashEntry(&local.fileHash, &sSearch);
        pEntry;
        pEntry = Tcl_NextHashEntry(&sSearch)
    ){
      ZvfsFile *pFile = (ZvfsFile *)Tcl_GetHashValue(pEntry);
      char *z = pFile->zName;
      Tcl_ListObjAppendElement(interp, pResult, Tcl_NewStringObj(z, -1));
    }
  }
  return TCL_OK;
}

/*
** Whenever a ZVFS file is opened, an instance of this structure is
** attached to the open channel where it will be available to the
** ZVFS I/O routines below.  All state information about an open
** ZVFS file is held in this structure.
*/
typedef struct ZvfsChannelInfo {
  unsigned long nByte;      /* number of bytes of uncompressed data */
  unsigned long nByteCompr; /* number of bytes of unread compressed data */
  unsigned long nData;      /* total number of bytes of compressed data */
  unsigned long readSoFar;  /* position of next byte to be read from the channel */
  int64_t startOfData;      /* File position of start of data in ZIP archive */
  Tcl_Channel chan;         /* Open file handle to the archive file */
  unsigned char *zBuf;      /* buffer used by the decompressor */
  unsigned char *uBuf;      /* pointer to the uncompressed, unencrypted data */
  z_stream stream;          /* state of the decompressor */
  int isEncrypted;          /* file is encrypted */
  int isCompressed;         /* True data is compressed */
} ZvfsChannelInfo;

/*
** This routine is called as an exit handler.  If we do not set
** ZvfsChannelInfo.chan to NULL, then Tcl_Close() will be called on
** that channel a second time when Tcl_Exit runs.  This will lead to a
** core dump.
*/
static void vfsExit(void *pArg) {
  ZvfsChannelInfo *pInfo = (ZvfsChannelInfo *)pArg;
  pInfo->chan = 0;
}

static void ZvfsFreeChannelInfo(ZvfsChannelInfo* pInfo) {
  if (pInfo->isCompressed) {
    inflateEnd(&pInfo->stream);
  }
  if (pInfo->zBuf) {
    Tcl_Free(pInfo->zBuf);
  }
  if (pInfo->uBuf) {
    Tcl_Free(pInfo->uBuf);
  }
  Tcl_Free((char *)pInfo);
}

/*
** This routine is called when the ZVFS channel is closed
*/
static int vfsClose(
  ClientData  instanceData,    /* A ZvfsChannelInfo structure */
  Tcl_Interp *interp           /* The TCL interpreter */
) {
  ZvfsChannelInfo* pInfo = (ZvfsChannelInfo*)instanceData;
  if (pInfo->chan) {
    Tcl_Close(interp, pInfo->chan);
    pInfo->chan = NULL;
    Tcl_DeleteExitHandler(vfsExit, pInfo);
  }
  ZvfsFreeChannelInfo(pInfo);
  return TCL_OK;
}

static int vfsInput(
  ClientData instanceData, /* The channel to read from */
  char *buf,               /* Buffer to fill */
  int toRead,              /* Requested number of bytes */
  int *pErrorCode          /* Location of error flag */
) { /* The TCL I/O system calls this function to actually read information
    * from a ZVFS file.
    */
  if (toRead == 0) {
    return 0;
  }

  ZvfsChannelInfo *pInfo = (ZvfsChannelInfo *)instanceData;
  unsigned long nextpos = pInfo->readSoFar + toRead;
  if (nextpos > pInfo->nByte) {
    toRead = pInfo->nByte - pInfo->readSoFar;
    nextpos = pInfo->nByte;
  }

  memcpy(buf, pInfo->uBuf + pInfo->readSoFar, toRead);
  pInfo->readSoFar = nextpos;
  *pErrorCode = 0;

  return toRead;
}


static int vfsRead(
  ClientData instanceData, /* The channel to read from */
  char *buf,               /* Buffer to fill */
  int toRead,              /* Requested number of bytes */
  int *pErrorCode          /* Location of error flag */
) { /* Read and decompress all data for the associated file into the specified buffer */
  ZvfsChannelInfo *pInfo = (ZvfsChannelInfo *)instanceData;

  if ((unsigned long)toRead > pInfo->nByte) {
    toRead = pInfo->nByte;
  }
  if (toRead == 0) {
    return 0;
  }
  if (pInfo->isEncrypted) {
    *pErrorCode = EINVAL;
    return -1;
  }

  if (pInfo->isCompressed) {
    int err = Z_OK;
    z_stream *stream = &pInfo->stream;
    stream->next_out = buf;
    stream->avail_out = toRead;
    while (stream->avail_out) {
      if (!stream->avail_in) {
        int len = pInfo->nByteCompr;
        if (len > COMPR_BUF_SIZE) {
          len = COMPR_BUF_SIZE;
        }
        int nread = Tcl_Read(pInfo->chan, pInfo->zBuf, len);
        if (nread != len) {
          *pErrorCode = Tcl_GetErrno();
          return -1;
        }

        pInfo->nByteCompr -= len;
        stream->next_in = pInfo->zBuf;
        stream->avail_in = len;
      }
      err = inflate(stream, Z_NO_FLUSH);
      if (err) {
        break;
      }
    }

    if (err == Z_STREAM_END) {
      if ((stream->avail_out != 0)) {
        *pErrorCode = err; /* premature end */
        return -1;
      }
    } else if (err) {
      *pErrorCode = err; /* some other zlib error */
      return -1;
    }
  } else {
    /* not compressed */
    int nread = Tcl_Read(pInfo->chan, buf, toRead);
    if (nread != toRead) {
      *pErrorCode = Tcl_GetErrno();
      return -1;
    }
  }

  // OK
  pInfo->nByte = toRead;
  pInfo->readSoFar = 0;
  *pErrorCode = 0;
  return toRead;
}

/*
** Write to a ZVFS file.  ZVFS files are always read-only, so this routine
** always returns an error.
*/
static int vfsOutput(
  ClientData instanceData,   /* The channel to write to */
  CONST char *buf,                 /* Data to be stored. */
  int toWrite,               /* Number of bytes to write. */
  int *pErrorCode            /* Location of error flag. */
) {
  *pErrorCode = EINVAL;
  return -1;
}

static int vfsSeek(
  ClientData instanceData,    /* The file structure */
  long offset,                /* Offset to seek to */
  int mode,                   /* One of SEEK_CUR, SEEK_SET or SEEK_END */
  int *pErrorCode             /* Write the error code here */
) { /* Move the file pointer so that the next byte read will be "offset". */
  ZvfsChannelInfo* pInfo = (ZvfsChannelInfo*)instanceData;

  switch (mode) {
  case SEEK_CUR:
    offset += pInfo->readSoFar;
    break;
  case SEEK_END:
    offset += pInfo->nByte - 1;
    break;
  default:
    /* Do nothing */
    break;
  }

  /* Don't seek past end of data */
  if (pInfo->nByte < (unsigned long)offset) {
    return -1;
  }

  /* Don't seek before the start of data */
  if (offset < 0) {
    return -1;
  }

  pInfo->readSoFar = (unsigned long)offset;
  return pInfo->readSoFar;
}

/*
** Handle events on the channel.  ZVFS files do not generate events,
** so this is a no-op.
*/
static void vfsWatchChannel(
  ClientData instanceData,   /* Channel to watch */
  int mask                   /* Events of interest */
) {
  return;
}

/*
** Called to retrieve the underlying file handle for this ZVFS file.
** As the ZVFS file has no underlying file handle, this is a no-op.
*/
static int vfsGetFile(
  ClientData  instanceData,   /* Channel to query */
  int direction,              /* Direction of interest */
  ClientData* handlePtr       /* Space to the handle into */
) {
  return TCL_ERROR;
}

/*
** This structure describes the channel type structure for
** access to the ZVFS.
*/
static Tcl_ChannelType vfsChannelType = {
  "vfs",            /* Type name.                                    */
  NULL,             /* Set blocking/nonblocking behaviour. NULL'able */
  vfsClose,         /* Close channel, clean instance data            */
  vfsInput,         /* Handle read request                           */
  vfsOutput,        /* Handle write request                          */
  vfsSeek,          /* Move location of access point.      NULL'able */
  NULL,             /* Set options.                        NULL'able */
  NULL,             /* Get options.                        NULL'able */
  vfsWatchChannel,  /* Initialize notifier                           */
  vfsGetFile        /* Get OS handle from the channel.               */
};

/*
** This routine attempts to do an open of a file.  Check to see
** if the file is located in the ZVFS.  If so, then open a channel
** for reading the file.  If not, return NULL.
*/
static Tcl_Channel ZvfsFileOpen(
  Tcl_Interp *interp,     /* The TCL interpreter doing the open */
  char *zFilename,        /* Name of the file to open */
  char *modeString,       /* Mode string for the open (ignored) */
  int permissions         /* Permissions for a newly created file (ignored) */
) {
  ZvfsChannelInfo *pInfo = NULL;
  static int count = 1;
  char zName[50];
  unsigned char zBuf[50];

  ZvfsFile *pFile = ZvfsLookup(zFilename);
  if (!pFile) {
    return NULL;
  }
  Tcl_Channel chan = Tcl_OpenFileChannel(interp, pFile->pArchive->zName, "r", 0);
  if (!chan) {
    return NULL;
  }

  if (Tcl_SetChannelOption(interp, chan, "-translation", "binary")
      || Tcl_SetChannelOption(interp, chan, "-encoding", "binary")
  ) {
    /* this should never happen */
    goto L_ERROR;
  }
  Tcl_Seek(chan, pFile->iOffset, SEEK_SET);
  if (30 != Tcl_Read(chan, zBuf, 30) || 0 != memcmp(zBuf, "\120\113\03\04", 4)) {
    Tcl_AppendResult(interp, "local header mismatch: ", NULL);
    goto L_ERROR;
  }

  pInfo = (ZvfsChannelInfo *)Tcl_Alloc(sizeof(*pInfo));
  pInfo->chan = chan;

  pInfo->isEncrypted = zBuf[6] & 1;
  if (pInfo->isEncrypted) {
    Tcl_AppendResult(interp, "encrypted file: ", NULL);
    goto L_ERROR;
  }

  pInfo->isCompressed = INT16(zBuf, 8);
  if (pInfo->isCompressed) {
    z_stream *stream = &pInfo->stream;
    pInfo->zBuf = Tcl_Alloc(COMPR_BUF_SIZE);
    stream->zalloc = (alloc_func)0;
    stream->zfree = (free_func)0;
    stream->opaque = (voidpf)0;
    stream->avail_in = 2;
    stream->next_in = pInfo->zBuf;
    pInfo->zBuf[0] = 0x78;
    pInfo->zBuf[1] = 0x01;
    inflateInit(&pInfo->stream);
  } else {
    pInfo->zBuf = 0;
  }
  pInfo->nByte = INT32(zBuf, 22);
  pInfo->nByteCompr = pInfo->nData = INT32(zBuf, 18);
  pInfo->readSoFar = 0;
  Tcl_Seek(chan, INT16(zBuf, 26) + INT16(zBuf, 28), SEEK_CUR);
  pInfo->startOfData = Tcl_Tell(chan);

  /* Read and decompress the file contents */
  {
    pInfo->uBuf = Tcl_Alloc(pInfo->nByte);
    int errCode = 0;
    int rc = vfsRead(pInfo, pInfo->uBuf, pInfo->nByte, &errCode);
    if (rc < 0) {
      Tcl_AppendResult(interp, "vfsRead error: ", NULL);
      goto L_ERROR;
    }
  }

  // OK
  Tcl_CreateExitHandler(vfsExit, pInfo);
  sprintf(zName, "vfs_%"PRIx64"_%"PRIx64"", ((uint64_t)(uintptr_t)pFile) >> 12, (uint64_t)count++);
  return Tcl_CreateChannel(&vfsChannelType, zName, (ClientData)pInfo, TCL_READABLE);

L_ERROR:
  if (chan) {
    Tcl_Close(interp, chan);
  }
  if (pInfo) {
    pInfo->chan = NULL;   // pInfo->chan was closed
    ZvfsFreeChannelInfo(pInfo);
  }
  return NULL;
}

/*
** This routine does a stat() system call for a ZVFS file.
*/
static int ZvfsFileStat(char *path, Tcl_StatBuf *buf) {

  ZvfsFile *pFile = ZvfsLookup(path);
  if (!pFile) {
    return -1;
  }
  memset(buf, 0, sizeof(Tcl_StatBuf));
  if (pFile->isdir) {
    buf->st_mode = 040555;
  } else {
    buf->st_mode = 0100555;
  }
  buf->st_size = pFile->nByte;
  buf->st_mtime = pFile->timestamp;
  buf->st_ctime = pFile->timestamp;
  buf->st_atime = pFile->timestamp;
  return 0;
}

/*
** This routine does an access() system call for a ZVFS file.
*/
static int ZvfsFileAccess(char *path, int mode) {
  if (mode & 3) {
    return -1;
  }
  ZvfsFile *pFile = ZvfsLookup(path);
  if (!pFile) {
    return -1;
  }
  return 0;
}

Tcl_Channel Tobe_FSOpenFileChannelProc _ANSI_ARGS_((
  Tcl_Interp *interp, Tcl_Obj *pathPtr,
  int mode, int permissions
)) {
  int len;
  /* if (mode != O_RDONLY) return NULL; */
  return ZvfsFileOpen(interp, Tcl_GetStringFromObj(pathPtr, &len), 0, permissions);
}

/*
** This routine does a stat() system call for a ZVFS file.
*/
int Tobe_FSStatProc _ANSI_ARGS_((Tcl_Obj *pathPtr, Tcl_StatBuf *buf)) {
  int len;
  return ZvfsFileStat(Tcl_GetStringFromObj(pathPtr, &len), buf);
}

/*
** This routine does an access() system call for a ZVFS file.
*/
int Tobe_FSAccessProc _ANSI_ARGS_((Tcl_Obj *pathPtr, int mode)) {
  int len;
  return ZvfsFileAccess(Tcl_GetStringFromObj(pathPtr, &len), mode);
}


/* Tcl_Obj* Tobe_FSFilesystemSeparatorProc
                            _ANSI_ARGS_((Tcl_Obj *pathPtr)) {
  return Tcl_NewStringObj("/",-1);;
} */
/* Function to process a
* 'Tobe_FSMatchInDirectory()'.  If not
* implemented, then glob and recursive
* copy functionality will be lacking in
* the filesystem. */
int Tobe_FSMatchInDirectoryProc _ANSI_ARGS_((
  Tcl_Interp* interp,
  Tcl_Obj *result, Tcl_Obj *pathPtr, CONST char *pattern,
  Tcl_GlobTypeData * types
)) {
  Tcl_HashEntry *pEntry;
  Tcl_HashSearch sSearch;
  int len = 0;
  int offset = 0;
  char *zPattern = NULL;
  char *z = Tcl_GetStringFromObj(pathPtr, &len);
  char tbuf[20000];

  if (!pattern) {
    return TCL_ERROR;
  }
  int l = (int)strlen(pattern);
  if (!z) {
    zPattern = strdup(pattern);
  } else {
    zPattern = (char *)malloc(len + l + 2);
    memcpy(zPattern, z , len);
    if (len > 1 || zPattern[0] != '/') {
      zPattern[len] = '/';
      ++len;
    }
    memcpy(zPattern + len, pattern, l + 1);
  }
  char *zPattern0 = zPattern;
#ifdef __WIN32__
  if (l && zPattern[1] == ':') {
     zPattern += 2;
     offset = 2;
  }
#endif
  int scnt = strchrcnt(zPattern, '/');
  for (
    pEntry = Tcl_FirstHashEntry(&local.fileHash, &sSearch);
    pEntry != NULL;
    pEntry = Tcl_NextHashEntry(&sSearch)
  ) {
    ZvfsFile *pFile = Tcl_GetHashValue(pEntry);
    if (Tcl_StringCaseMatch(pFile->zName, zPattern, 0) && scnt == pFile->depth) {
#ifdef __WIN32__
      tbuf[0] = ' '; tbuf[1]= ' ';
      strcpy(tbuf + offset, pFile->zName);
#else
      strcpy(tbuf, zpFile->zName);
#endif
      Tcl_ListObjAppendElement(interp, result, Tcl_NewStringObj(tbuf, -1));
    }
  }
  free(zPattern0);
  return TCL_OK;
}

/* Function to check whether a path is in
* this filesystem.  This is the most
* important filesystem procedure. */
int Tobe_FSPathInFilesystemProc _ANSI_ARGS_((
  Tcl_Obj *pathPtr, ClientData *clientDataPtr
)) {
  int len;
  char *path = Tcl_GetStringFromObj(pathPtr, &len);
  if (ZvfsLookup(path)) {
    return TCL_OK;
  }
  return -1;
}

Tcl_Obj *Tobe_FSListVolumesProc _ANSI_ARGS_((void)) {
  Tcl_Obj *pVols = NULL;

  Tcl_HashSearch zSearch;   /* Search all mount points */
  Tcl_HashEntry *pEntry = Tcl_FirstHashEntry(&local.archiveHash, &zSearch);
  while (pEntry) {
    ZvfsArchive *pArchive = Tcl_GetHashValue(pEntry);
    if (pArchive) {
      if (!pVols) {
        pVols = Tcl_NewListObj(0, 0);
        Tcl_IncrRefCount(pVols);
      }
      char *mountpt = Tcl_Alloc((unsigned int)(strlen("zvfs:") + strlen(pArchive->zMountPoint) + 1));
      sprintf(mountpt, "zvfs:%s", pArchive->zMountPoint);
      Tcl_Obj *pVol = Tcl_NewStringObj(mountpt, -1);
      Tcl_Free(mountpt);
      Tcl_IncrRefCount(pVol);
      Tcl_ListObjAppendElement(NULL, pVols, pVol);
      /* Tcl_AppendResult(interp,pArchive->zMountPoint," ",pArchive->zName," ",0);*/
    }
    pEntry = Tcl_NextHashEntry(&zSearch);
  }
  return pVols;
}

int Tobe_FSChdirProc _ANSI_ARGS_((Tcl_Obj *pathPtr)) {
   /* Someday, we should actually check if this is a valid path. */
   return TCL_OK;
}

static const char *TobeAttrs[] = { "uncompsize", "compsize", "offset", "mount", "archive", 0 };

const char *CONST86 * Tobe_FSFileAttrStringsProc _ANSI_ARGS_((
  Tcl_Obj *pathPtr, Tcl_Obj** objPtrRef
)) {
  return TobeAttrs;
}

int Tobe_FSFileAttrsGetProc _ANSI_ARGS_((
  Tcl_Interp *interp,
  int index, Tcl_Obj *pathPtr,
  Tcl_Obj **objPtrRef
)) {
  char *zFilename = Tcl_GetString(pathPtr);
  ZvfsFile *pFile = ZvfsLookup(zFilename);
  if(!pFile) {
    return TCL_ERROR;
  }

  switch (index) {
  case 0:
    *objPtrRef = Tcl_NewIntObj(pFile->nByteCompr);
    return TCL_OK;
  case 1:
    *objPtrRef = Tcl_NewIntObj(pFile->nByte);
    return TCL_OK;
  case 2:
    *objPtrRef = Tcl_NewIntObj(pFile->nByte);
    return TCL_OK;
  case 3:
    *objPtrRef = Tcl_NewStringObj(pFile->pArchive->zMountPoint, -1);
    return TCL_OK;
  case 4:
    *objPtrRef = Tcl_NewStringObj(pFile->pArchive->zName, -1);
    return TCL_OK;
  default:
    return TCL_ERROR;
  }
  return TCL_OK;
}

int Tobe_FSFileAttrsSetProc _ANSI_ARGS_((
  Tcl_Interp *interp,
  int index, Tcl_Obj *pathPtr,
  Tcl_Obj *objPtr
)) {
  return TCL_ERROR;
}

Tcl_Obj* Tobe_FSFilesystemPathTypeProc _ANSI_ARGS_((Tcl_Obj *pathPtr)) {
    return Tcl_NewStringObj("zip", -1);
}
/****************************************************/

// At some point, some of the following might get implemented?

#define Tobe_FSFilesystemSeparatorProc 0
#define Tobe_FSLoadFileProc 0
#define Tobe_FSUnloadFileProc 0
#define Tobe_FSGetCwdProc 0
#define Tobe_FSGetCwdProc 0
#define Tobe_FSCreateDirectoryProc 0
#define Tobe_FSDeleteFileProc 0
#define Tobe_FSCopyDirectoryProc 0
#define Tobe_FSCopyFileProc 0
#define Tobe_FSRemoveDirectoryProc 0
#define Tobe_FSNormalizePathProc 0
#define Tobe_FSUtimeProc 0
#define Tobe_FSRenameFileProc 0
#define Tobe_FSCreateInternalRepProc 0
#define Tobe_FSInternalToNormalizedProc 0
#define Tobe_FSDupInternalRepProc 0
#define Tobe_FSFreeInternalRepProc 0
#define Tobe_FSLinkProc  0

#if 0

int Tobe_FSLoadFileProc _ANSI_ARGS_((Tcl_Interp * interp,
                            Tcl_Obj *pathPtr, char * sym1, char * sym2,
                            Tcl_PackageInitProc ** proc1Ptr,
                            Tcl_PackageInitProc ** proc2Ptr,
                            ClientData * clientDataPtr)) { return 0; }

void Tobe_FSUnloadFileProc _ANSI_ARGS_((ClientData clientData)) {
  return;
}

int Tobe_FSNormalizePathProc _ANSI_ARGS_((Tcl_Interp *interp,
                         Tcl_Obj *pathPtr, int nextCheckpoint)) { return 0; }
Tcl_Obj* Tobe_FSGetCwdProc _ANSI_ARGS_((Tcl_Interp *interp)) { return 0; }
int Tobe_FSCreateDirectoryProc _ANSI_ARGS_((Tcl_Obj *pathPtr)) { return 0; }
int Tobe_FSDeleteFileProc _ANSI_ARGS_((Tcl_Obj *pathPtr)) { return 0; }
int Tobe_FSCopyDirectoryProc _ANSI_ARGS_((Tcl_Obj *srcPathPtr,
           Tcl_Obj *destPathPtr, Tcl_Obj **errorPtr)) { return 0; }
int Tobe_FSCopyFileProc _ANSI_ARGS_((Tcl_Obj *srcPathPtr,
                            Tcl_Obj *destPathPtr)) { return 0; }
int Tobe_FSRemoveDirectoryProc _ANSI_ARGS_((Tcl_Obj *pathPtr,
                            int recursive, Tcl_Obj **errorPtr)) { return 0; }
int Tobe_FSRenameFileProc _ANSI_ARGS_((Tcl_Obj *srcPathPtr,
                            Tcl_Obj *destPathPtr)) { return 0; }
/* We have to declare the utime structure here. */
int Tobe_FSUtimeProc _ANSI_ARGS_((Tcl_Obj *pathPtr,
                                           struct utimbuf *tval)) { return 0; }
Tcl_Obj* Tobe_FSLinkProc _ANSI_ARGS_((Tcl_Obj *pathPtr)) { return 0; }
void Tobe_FSFreeInternalRepProc _ANSI_ARGS_((ClientData clientData)) { return; }
ClientData Tobe_FSDupInternalRepProc
                            _ANSI_ARGS_((ClientData clientData)) { return 0; }
Tcl_Obj* Tobe_FSInternalToNormalizedProc
                            _ANSI_ARGS_((ClientData clientData)) { return 0; }
ClientData Tobe_FSCreateInternalRepProc _ANSI_ARGS_((Tcl_Obj *pathPtr)) {
  return 0;
}

#endif


static Tcl_Filesystem Tobe_Filesystem = {
    "zvfs",                 /* The name of the filesystem. */
    sizeof(Tcl_Filesystem), /* Length of this structure, so future
                             * binary compatibility can be assured. */
    TCL_FILESYSTEM_VERSION_1,
                            /* Version of the filesystem type. */
    Tobe_FSPathInFilesystemProc,
                            /* Function to check whether a path is in
                             * this filesystem.  This is the most
                             * important filesystem procedure. */
    Tobe_FSDupInternalRepProc,
                            /* Function to duplicate internal fs rep.  May
                             * be NULL (but then fs is less efficient). */
    Tobe_FSFreeInternalRepProc,
                            /* Function to free internal fs rep.  Must
                             * be implemented, if internal representations
                             * need freeing, otherwise it can be NULL. */
    Tobe_FSInternalToNormalizedProc,
                            /* Function to convert internal representation
                             * to a normalized path.  Only required if
                             * the fs creates pure path objects with no
                             * string/path representation. */
    Tobe_FSCreateInternalRepProc,
                            /* Function to create a filesystem-specific
                             * internal representation.  May be NULL
                             * if paths have no internal representation,
                             * or if the Tobe_FSPathInFilesystemProc
                             * for this filesystem always immediately
                             * creates an internal representation for
                             * paths it accepts. */
    Tobe_FSNormalizePathProc,
                            /* Function to normalize a path.  Should
                             * be implemented for all filesystems
                             * which can have multiple string
                             * representations for the same path
                             * object. */
    Tobe_FSFilesystemPathTypeProc,
                            /* Function to determine the type of a
                             * path in this filesystem.  May be NULL. */
    Tobe_FSFilesystemSeparatorProc,
                            /* Function to return the separator
                             * character(s) for this filesystem.  Must
                             * be implemented. */
    Tobe_FSStatProc,
                            /*
                             * Function to process a 'Tobe_FSStat()'
                             * call.  Must be implemented for any
                             * reasonable filesystem.
                             */
    Tobe_FSAccessProc,
                            /*
                             * Function to process a 'Tobe_FSAccess()'
                             * call.  Must be implemented for any
                             * reasonable filesystem.
                             */
    Tobe_FSOpenFileChannelProc,
                            /*
                             * Function to process a
                             * 'Tobe_FSOpenFileChannel()' call.  Must be
                             * implemented for any reasonable
                             * filesystem.
                             */
    Tobe_FSMatchInDirectoryProc,
                            /* Function to process a
                             * 'Tobe_FSMatchInDirectory()'.  If not
                             * implemented, then glob and recursive
                             * copy functionality will be lacking in
                             * the filesystem. */
    Tobe_FSUtimeProc,
                            /* Function to process a
                             * 'Tobe_FSUtime()' call.  Required to
                             * allow setting (not reading) of times
                             * with 'file mtime', 'file atime' and
                             * the open-r/open-w/fcopy implementation
                             * of 'file copy'. */
    Tobe_FSLinkProc,
                            /* Function to process a
                             * 'Tobe_FSLink()' call.  Should be
                             * implemented only if the filesystem supports
                             * links. */
    Tobe_FSListVolumesProc,
                            /* Function to list any filesystem volumes
                             * added by this filesystem.  Should be
                             * implemented only if the filesystem adds
                             * volumes at the head of the filesystem. */
    Tobe_FSFileAttrStringsProc,
                            /* Function to list all attributes strings
                             * which are valid for this filesystem.
                             * If not implemented the filesystem will
                             * not support the 'file attributes' command.
                             * This allows arbitrary additional information
                             * to be attached to files in the filesystem. */
    Tobe_FSFileAttrsGetProc,
                            /* Function to process a
                             * 'Tobe_FSFileAttrsGet()' call, used by
                             * 'file attributes'. */
    Tobe_FSFileAttrsSetProc,
                            /* Function to process a
                             * 'Tobe_FSFileAttrsSet()' call, used by
                             * 'file attributes'.  */
    Tobe_FSCreateDirectoryProc,
                            /* Function to process a
                             * 'Tobe_FSCreateDirectory()' call. Should
                             * be implemented unless the FS is
                             * read-only. */
    Tobe_FSRemoveDirectoryProc,
                            /* Function to process a
                             * 'Tobe_FSRemoveDirectory()' call. Should
                             * be implemented unless the FS is
                             * read-only. */
    Tobe_FSDeleteFileProc,
                            /* Function to process a
                             * 'Tobe_FSDeleteFile()' call.  Should
                             * be implemented unless the FS is
                             * read-only. */
    Tobe_FSCopyFileProc,
                            /* Function to process a
                             * 'Tobe_FSCopyFile()' call.  If not
                             * implemented Tcl will fall back
                             * on open-r, open-w and fcopy as
                             * a copying mechanism. */
    Tobe_FSRenameFileProc,
                            /* Function to process a
                             * 'Tobe_FSRenameFile()' call.  If not
                             * implemented, Tcl will fall back on
                             * a copy and delete mechanism. */
    Tobe_FSCopyDirectoryProc,
                            /* Function to process a
                             * 'Tobe_FSCopyDirectory()' call.  If
                             * not implemented, Tcl will fall back
                             * on a recursive create-dir, file copy
                             * mechanism. */
    Tobe_FSLoadFileProc,
                            /* Function to process a
                             * 'Tobe_FSLoadFile()' call.  If not
                             * implemented, Tcl will fall back on
                             * a copy to native-temp followed by a
                             * Tobe_FSLoadFile on that temporary copy. */
    Tobe_FSUnloadFileProc,
                            /* Function to unload a previously
                             * successfully loaded file.  If load was
                             * implemented, then this should also be
                             * implemented, if there is any cleanup
                             * action required. */
    Tobe_FSGetCwdProc,
                            /*
                             * Function to process a 'Tobe_FSGetCwd()'
                             * call.  Most filesystems need not
                             * implement this.  It will usually only be
                             * called once, if 'getcwd' is called
                             * before 'chdir'.  May be NULL.
                             */
    Tobe_FSChdirProc,
                            /*
                             * Function to process a 'Tobe_FSChdir()'
                             * call.  If filesystems do not implement
                             * this, it will be emulated by a series of
                             * directory access checks.  Otherwise,
                             * virtual filesystems which do implement
                             * it need only respond with a positive
                             * return result if the dirName is a valid
                             * directory in their filesystem.  They
                             * need not remember the result, since that
                             * will be automatically remembered for use
                             * by GetCwd.  Real filesystems should
                             * carry out the correct action (i.e. call
                             * the correct system 'chdir' api).  If not
                             * implemented, then 'cd' and 'pwd' will
                             * fail inside the filesystem.
                             */
};

void (*Zvfs_PostInit)(Tcl_Interp *)=0;

/*
** Initialize the ZVFS system.
*/
int Zvfs_doInit(Tcl_Interp *interp, int safe){
  int n;
#ifdef USE_TCL_STUBS
  if( Tcl_InitStubs(interp,"8.0",0)==0 ){
    return TCL_ERROR;
  }
#endif
  Tcl_PkgProvide(interp, "zvfs", "1.0");
  if (!safe) {
    Tcl_CreateCommand(interp, "zvfs::mount", ZvfsMountCmd, 0, 0);
    Tcl_CreateCommand(interp, "zvfs::unmount", ZvfsUnmountCmd, 0, 0);
  }
  if (!local.isInit) {
    /* One-time initialization of the ZVFS */
    n = Tcl_FSRegister(0, &Tobe_Filesystem);
    Tcl_InitHashTable(&local.fileHash, TCL_STRING_KEYS);
    Tcl_InitHashTable(&local.archiveHash, TCL_STRING_KEYS);
    Tcl_CreateObjCommand(interp, "zvfs::exists", ZvfsExistsObjCmd, 0, 0);
    Tcl_CreateObjCommand(interp, "zvfs::info", ZvfsInfoObjCmd, 0, 0);
    Tcl_CreateObjCommand(interp, "zvfs::list", ZvfsListObjCmd, 0, 0);

    local.isInit = 1;
  }
  if (Zvfs_PostInit) Zvfs_PostInit(interp);
  return TCL_OK;
}

int Zvfs_Init(Tcl_Interp *interp){
  return Zvfs_doInit(interp, 0);
}

int Zvfs_SafeInit(Tcl_Interp *interp){
  return Zvfs_doInit(interp, 1);
}

