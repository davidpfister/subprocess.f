#pragma once

#include <cstddef>
#include <Windows.h>

#ifdef _MSC_VER
#define SUBPROCESS_CDECL         __cdecl
#define SUBPROCESS_API           __declspec(dllexport)

#else
#define THREADING_CDECL
#define THREADING_API
#endif // _MSC_VER

#define SUBPROCESS_C_API(RET_TYPE) extern "C" SUBPROCESS_API RET_TYPE SUBPROCESS_CDECL

SUBPROCESS_C_API(HANDLE) winpopen(char[], char[], DWORD&);

SUBPROCESS_C_API(int) winpclose(HANDLE);

SUBPROCESS_C_API(int) winfflush(HANDLE);

SUBPROCESS_C_API(HANDLE) winfgets(char*, unsigned int&, HANDLE);

SUBPROCESS_C_API(int) winkill(DWORD, DWORD);

/*EOF*/