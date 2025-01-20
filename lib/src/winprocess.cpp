// subprocess.cpp : Defines the functions for the static library.
//
#ifdef _WIN32
#define OS_WIN 1
#endif

#ifdef _WIN64
#define OS_WIN 1
#endif

#ifndef OS_WIN
#define OS_LINUX 1
#endif

#ifdef OS_WIN
#ifndef WIN32_LEAN_AND_MEAN
#	define WIN32_LEAN_AND_MEAN
#endif
#include <errno.h>
#include <io.h>
#include <ctype.h>
#include <direct.h>
#include <windows.h>
#include <WinNT.h>
#include <setjmp.h>
#include <intrin.h>
#include <stdbool.h>
#include <sys/timeb.h>
#ifndef _FPM
#include <atlstr.h>
#endif
#include <process.h>
#include <stdio.h>
#include <string>
#include "winprocess.h"


#define BUFSIZE 4096 

inline BOOL CloseHandleSafe(HANDLE h)
{
    h = nullptr;
    BOOL res;
    res = h && h != INVALID_HANDLE_VALUE ? CloseHandle(h) : TRUE;
    return res;
}

extern HANDLE winpopen(char command[], char mode[], DWORD& pid)
// Create a child process that uses the previously created pipes for STDIN and STDOUT.
{
    HANDLE pipe_write = nullptr;
    HANDLE pipe_read = nullptr;
    SECURITY_ATTRIBUTES security_attrs;
    STARTUPINFO startup_info;
    PROCESS_INFORMATION proc_info;
    BOOL bSuccess = FALSE;

    security_attrs.nLength = sizeof(security_attrs);
    security_attrs.lpSecurityDescriptor = nullptr;
    security_attrs.bInheritHandle = 1;

    bSuccess = CreatePipe(&pipe_read, &pipe_write, &security_attrs, 0);

    if (!bSuccess) {
        return nullptr;
    }

    bSuccess = SetHandleInformation(pipe_read, HANDLE_FLAG_INHERIT, 0);

    if (!bSuccess) {
        return nullptr;
    }

    // Set up members of the PROCESS_INFORMATION structure. 

    ZeroMemory(&proc_info, sizeof(PROCESS_INFORMATION));

    // Set up members of the STARTUPINFO structure. 
    // This structure specifies the STDIN and STDOUT handles for redirection.

    ZeroMemory(&startup_info, sizeof(STARTUPINFO));
    startup_info.cb = sizeof(STARTUPINFO);
    startup_info.lpReserved = 0;
    startup_info.lpDesktop = 0;
    startup_info.lpTitle = 0;
    startup_info.dwX = 0;
    startup_info.dwY = 0;
    startup_info.dwXSize = 0;
    startup_info.dwYSize = 0;
    startup_info.dwXCountChars = 0;
    startup_info.dwYCountChars = 0;
    startup_info.dwFillAttribute = 0;
    startup_info.dwFlags = STARTF_USESTDHANDLES;
    startup_info.wShowWindow = SW_HIDE;
    startup_info.cbReserved2 = 0;
    startup_info.lpReserved2 = 0;
    startup_info.hStdInput = 0;
    startup_info.hStdOutput = pipe_write;
    startup_info.hStdError = 0;

    // Create the child process. 

    bSuccess = CreateProcess(NULL,
#ifdef _FPM
		(LPTSTR)command,
#else
		CA2CT(command),
#endif
        NULL,          // process security attributes 
        NULL,          // primary thread security attributes 
        TRUE,          // handles are inherited 
        0,             // creation flags 
        NULL,          // use parent's environment 
        NULL,          // use parent's current directory 
        &startup_info,  // STARTUPINFO pointer 
        &proc_info);  // receives PROCESS_INFORMATION
    
    if (!bSuccess) {
        return NULL;
    }
	WaitForSingleObject(proc_info.hProcess, INFINITE);
    
    pid = proc_info.dwProcessId;

    CloseHandle(proc_info.hProcess);
    CloseHandle(proc_info.hThread);
    CloseHandle(pipe_write);

    return pipe_read;
}

extern int winpclose(HANDLE h)  {
    BOOL bSuccess = FALSE;
    DWORD dw;
    dw = GetLastError();
    bSuccess = CloseHandleSafe(h);

    if (!bSuccess) {
        return (int)dw;
    }
    return 0;
}

extern int winfflush(HANDLE read_handle) {
    return 0;
}

extern HANDLE winfgets(char* line, unsigned int& size, HANDLE read_handle) {
    DWORD dwRead;
    BOOL bSuccess = FALSE;
    char chBuf[BUFSIZE];
    bSuccess = ReadFile(read_handle, chBuf, sizeof(chBuf), &dwRead, nullptr);

    if (!bSuccess || dwRead == 0) {
        CloseHandle(read_handle);
        return nullptr;
    }
    std::string str(chBuf);
    size = (int)str.find_last_of('\n') + 1;

    for (int i = 0; i < (int)str.length(); i++) {
        if (i < (int)size) 
        {
            line[i] = str[i];
        }
        else 
        {
            line[i] = char(0);
        }      
    }
    return read_handle;
}

extern int winkill(DWORD dwProcessId, DWORD signal) {
    DWORD dwDesiredAccess = PROCESS_TERMINATE;
    BOOL  bInheritHandle = FALSE;
    UINT uExitCode = NULL;
    HANDLE hProcess = OpenProcess(dwDesiredAccess, bInheritHandle, dwProcessId);

    TerminateProcess(hProcess, uExitCode);

    CloseHandle(hProcess);
    return 0;
}

#endif

/*EOF*/
