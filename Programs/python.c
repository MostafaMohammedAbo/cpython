/* Minimal main program -- everything is loaded from the library */

#include "osdefs.h" // MAXPATHLEN
#include "Python.h"
#include "pycore_pylifecycle.h"

#ifdef MS_WINDOWS
int
wmain(int argc, wchar_t **argv)
{
    // get exe path
    wchar_t program_full_path[MAXPATHLEN+1];
    memset(program_full_path, 0, sizeof(program_full_path));
    if (!GetModuleFileNameW(NULL, program_full_path, MAXPATHLEN)) {
        fprintf(stderr, "Cannot GetModuleFileNameW(NULL)");
        return -1;
    }

    // check zip signatrue
    // FIXME: support zip file with comment
    // END_CENTRAL_DIR_SIZE = 22
    // STRING_END_ARCHIVE = b'PK\x05\x06'
    HANDLE hfile = CreateFileW(
        program_full_path,
        GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        NULL
    );
    if (hfile == INVALID_HANDLE_VALUE) {
        fprintf(stderr, "Cannot open self file");
        return -2;
    }
    DWORD rc = SetFilePointer(hfile, -22, NULL, FILE_END);
    if (rc == INVALID_SET_FILE_POINTER) {
        (void)CloseHandle(hfile);
        fprintf(stderr, "Cannot seek self file");
        return -3;
    }
    char buf[4] = {0, 0, 0, 0};
    DWORD read_sz = 4;
    BOOL ok = ReadFile(hfile, (LPVOID)buf, read_sz, &read_sz, NULL);
    if (!ok) {
        (void)CloseHandle(hfile);
        fprintf(stderr, "Cannot read self file");
        return -4;
    }
    (void)CloseHandle(hfile);

    char sig[4] = {'P', 'K', 0x05, 0x06};
    if (0 == memcmp(buf, sig, 4)) {
        // found zip sig
        Py_SetPath(program_full_path);
    }

    return Py_Main(argc, argv);
}
#else
int
main(int argc, char **argv)
{
    return Py_BytesMain(argc, argv);
}
#endif
