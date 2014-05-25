#if defined _WIN32 || defined _WIN64
#include <windows.h>
#endif

#include "MatchTempl.hpp"

#if defined _WIN32 || defined _WIN64
#ifdef __cplusplus
extern "C"
{
#endif

bool __stdcall DllMain(HINSTANCE hinstDLL, DWORD fdwReason, void* lpvReserved)
{
    switch (fdwReason)
    {
        case DLL_PROCESS_ATTACH:  //module first loaded..
            module = hinstDLL;
            break;

        case DLL_PROCESS_DETACH:  //module unloaded..
            break;

        case DLL_THREAD_ATTACH:  //script started..
            break;

        case DLL_THREAD_DETACH:  //script finished..
            break;
    }

    return true;
}

#ifdef __cplusplus
}
#endif

#else

void load() __attribute__((constructor))
{
    //linux code.. module loaded.
}

void unload() __attribute__((destructor))
{
    //linux code.. module unloaded..
}
#endif
