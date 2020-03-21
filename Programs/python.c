/* Minimal main program -- everything is loaded from the library */

#include "Python.h"
#include "pycore_pylifecycle.h"


#ifdef MS_WINDOWS
static int
wmain_bootstrap(int argc, wchar_t **argv) {
    PyStatus status;

    PyConfig config;
    PyConfig_InitPythonConfig(&config);
    /* always isolated */
    config.isolated = 1;
    /* set bootstrap module */
    config.run_module = _PyMem_RawWcsdup(L"__bootstrap__");

    /* Decode command line arguments.
       Implicitly preinitialize Python (in isolated mode). */
    status = PyConfig_SetArgv(&config, argc, argv);
    if (PyStatus_Exception(status)) {
        goto fail;
    }

    status = Py_InitializeFromConfig(&config);
    if (PyStatus_Exception(status)) {
        goto fail;
    }
    PyConfig_Clear(&config);

    return Py_RunMain();

fail:
    PyConfig_Clear(&config);
    if (PyStatus_IsExit(status)) {
        return status.exitcode;
    }
    /* Display the error message and exit the process with
       non-zero exit code */
    Py_ExitStatusException(status);
    return -1;
}

int
wmain(int argc, wchar_t **argv)
{
    extern wchar_t *_Py_GetStandaloneExecutable();
    extern int _Py_HaveBootstrapModule();

    wchar_t *sa_exe = _Py_GetStandaloneExecutable();
    if (sa_exe != NULL) {
        Py_SetPath(sa_exe);
    }
    if (_Py_HaveBootstrapModule()) {
        return wmain_bootstrap(argc, argv);
    } else {
        return Py_Main(argc, argv);
    }
}
#else
int
main(int argc, char **argv)
{
    return Py_BytesMain(argc, argv);
}
#endif
