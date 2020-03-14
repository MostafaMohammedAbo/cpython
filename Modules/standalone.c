#include "osdefs.h" // MAXPATHLEN
#include "Python.h"

#ifdef MS_WINDOWS
#include <windows.h>
#endif


static int _py_is_sa = -1;
static const wchar_t *_py_sa_path = NULL;
static wchar_t _py_sa_path_storage[MAXPATHLEN+1];

int _Py_IsStandalone_impl() {
    // get exe path
    memset(_py_sa_path_storage, 0, sizeof(_py_sa_path_storage));

#ifdef MS_WINDOWS
    if (!GetModuleFileNameW(NULL, _py_sa_path_storage, MAXPATHLEN)) {
        fprintf(stderr, "Cannot GetModuleFileNameW(NULL)");
        return 0;
    }
#else
    // TOOD: non windows
    return 0;
#endif

    FILE *fp = _Py_wfopen(_py_sa_path_storage, L"rb");
    if (!fp) {
        return 0;
    }

    // seek and read sig
    int rc = fseek(fp, -22, SEEK_END);
    if (rc != 0) {
        (void)fclose(fp);
        return 0;
    }
    char buf[4] = {0, 0, 0, 0};
    (void)fread(buf, 4, 1, fp);
    (void)fclose(fp);

    if (memcmp(buf, "PK\x05\x06", 4) == 0) {
        _py_sa_path = _py_sa_path_storage;
        return 1;
    }
    return 0;
}

const wchar_t *_Py_GetStandaloneExecutable() {
    if (_py_is_sa < 0) {
        _py_is_sa = _Py_IsStandalone_impl();
    }
    return _py_sa_path;
}
