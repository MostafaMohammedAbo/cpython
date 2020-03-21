#include "osdefs.h" // MAXPATHLEN
#include "Python.h"

#ifdef MS_WINDOWS
#include <windows.h>
#endif


#define END_CENTRAL_DIR_SIZE 22
#define CENTRAL_DIR_HEADER_SIZE 46


static int _py_sa_init = 0;
static int _py_sa_have_bootstrap = 0;
static const wchar_t *_py_sa_path = NULL;
static wchar_t _py_sa_path_storage[MAXPATHLEN+1];


static uint32_t _unpack_uint32(const uint8_t *data) {
    // return *(uint32_t *)(data + offset);
    return (((uint32_t)data[0]) << 0) | (((uint32_t)data[1]) << 8)
        | (((uint32_t)data[2]) << 16) | (((uint32_t)data[3]) << 24);
}

static uint16_t _unpack_uint16(const uint8_t *data) {
    // return *(uint16_t *)(data + offset);
    return ((uint16_t)data[0]) | (((uint16_t)data[1]) << 8);
}


// ref: Lib/zipimport.py
static void _Py_IsStandalone_impl() {
    // get exe path
    memset(_py_sa_path_storage, 0, sizeof(_py_sa_path_storage));

#ifdef MS_WINDOWS
    if (!GetModuleFileNameW(NULL, _py_sa_path_storage, MAXPATHLEN)) {
        fprintf(stderr, "Cannot GetModuleFileNameW(NULL)");
        return;
    }
#else
    // TOOD: non windows
    return;
#endif

    FILE *fp = _Py_wfopen(_py_sa_path_storage, L"rb");
    if (!fp) {
        return;
    }

    // seek to end of central dir
    int rc = fseek(fp, -END_CENTRAL_DIR_SIZE, SEEK_END);
    if (rc != 0) {
        goto done;
    }
    // read end of central dir
    uint8_t buf[CENTRAL_DIR_HEADER_SIZE];
    ssize_t nread = fread(buf, 1, END_CENTRAL_DIR_SIZE, fp);
    if (nread != END_CENTRAL_DIR_SIZE) {
        goto done;
    }
    if (memcmp(buf, "PK\x05\x06", 4) != 0) {
        goto done;
    }

    // found zip file
    _py_sa_path = _py_sa_path_storage;

    // seek to Start of Central Directory
    {
        uint32_t header_size = _unpack_uint32(buf + 12);
        if (0 != fseek(fp, -END_CENTRAL_DIR_SIZE - (int32_t)header_size, SEEK_END)) {
            goto done;
        }
    }
    // read each record
    for (uint16_t nfile = _unpack_uint16(buf + 8); nfile > 0; --nfile) {
        nread = fread(buf, 1, CENTRAL_DIR_HEADER_SIZE, fp);
        if (nread != CENTRAL_DIR_HEADER_SIZE) {
            goto done;
        }
        // Start of file header
        if (0 != memcmp(buf, "PK\x01\x02", 4)) {
            goto done;
        }
        uint16_t name_size = _unpack_uint16(buf + 28);
        uint16_t extra_size = _unpack_uint16(buf + 30);
        uint16_t comment_size = _unpack_uint16(buf + 32);
        uint32_t header_size = name_size + extra_size + comment_size;
        uint8_t header_buf[1024];
        if (header_size + 1 > sizeof(header_buf)) {
            // header too long
            goto done;
        }
        // read remaining header
        nread = fread(header_buf, 1, header_size, fp);
        if (nread != header_size) {
            goto done;
        }
        // name
        header_buf[name_size] = 0;

        // is bootstrap module?
        if (0 == strcmp(header_buf, "__bootstrap__.py")
            || 0 == strcmp(header_buf, "__bootstrap__.pyc"))
        {
            _py_sa_have_bootstrap = 1;
            break;
        }
    }   // read each record

done:
    if (fp) {
        (void)fclose(fp);
    }
}

const wchar_t *_Py_GetStandaloneExecutable() {
    if (_py_sa_init == 0) {
        _Py_IsStandalone_impl();
        _py_sa_init = 1;
    }
    return _py_sa_path;
}

int _Py_HaveBootstrapModule() {
    return _py_sa_have_bootstrap;
}
