/*******************************************************************************
 * system_stats_windows.c - Windows system statistics
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#include <psapi.h>
#include <tlhelp32.h>
#pragma comment(lib, "psapi.lib")

//==============================================================================
// PLATFORM DETECTION
//==============================================================================

int get_platform(void) {
    return 1;  // Windows
}

//==============================================================================
// CPU STATISTICS
//==============================================================================

static FILETIME prev_idle[128], prev_kernel[128], prev_user[128];
static int win_initialized = 0;

int get_num_cpu_cores(void) {
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwNumberOfProcessors;
}

int get_cpu_usage(float usage[], int max) {
    int num_cores = get_num_cpu_cores();
    if (num_cores > max) num_cores = max;

    FILETIME idle, kernel, user;
    if (GetSystemTimes(&idle, &kernel, &user)) {
        if (!win_initialized) {
            prev_idle[0] = idle;
            prev_kernel[0] = kernel;
            prev_user[0] = user;
            win_initialized = 1;
            for (int i = 0; i < num_cores; i++) usage[i] = 0.0f;
            return num_cores;
        }

        ULONGLONG idle_diff =
            (((ULONGLONG)idle.dwHighDateTime << 32) | idle.dwLowDateTime) -
            (((ULONGLONG)prev_idle[0].dwHighDateTime << 32) | prev_idle[0].dwLowDateTime);

        ULONGLONG kernel_diff =
            (((ULONGLONG)kernel.dwHighDateTime << 32) | kernel.dwLowDateTime) -
            (((ULONGLONG)prev_kernel[0].dwHighDateTime << 32) | prev_kernel[0].dwLowDateTime);

        ULONGLONG user_diff =
            (((ULONGLONG)user.dwHighDateTime << 32) | user.dwLowDateTime) -
            (((ULONGLONG)prev_user[0].dwHighDateTime << 32) | prev_user[0].dwLowDateTime);

        ULONGLONG total = kernel_diff + user_diff;
        float cpu_percent = total > 0 ? (float)(total - idle_diff) / total : 0.0f;

        for (int i = 0; i < num_cores; i++)
            usage[i] = cpu_percent;

        prev_idle[0] = idle;
        prev_kernel[0] = kernel;
        prev_user[0] = user;

        return num_cores;
    }

    for (int i = 0; i < num_cores; i++) usage[i] = 0.0f;
    return num_cores;
}

//==============================================================================
// MEMORY STATISTICS
//==============================================================================

void get_memory_detailed(int *total_mb, int *used_mb, int *free_mb,
                         int *avail_mb, int *buff_mb, int *cached_mb,
                         int *swap_total_mb, int *swap_used_mb) {
    MEMORYSTATUSEX memInfo;
    memInfo.dwLength = sizeof(MEMORYSTATUSEX);

    if (GlobalMemoryStatusEx(&memInfo)) {
        *total_mb = (int)(memInfo.ullTotalPhys / (1024 * 1024));
        *avail_mb = (int)(memInfo.ullAvailPhys / (1024 * 1024));
        *used_mb  = *total_mb - *avail_mb;
        *free_mb  = *avail_mb;
        *buff_mb  = 0;
        *cached_mb = 0;
        *swap_total_mb =
            (int)(memInfo.ullTotalPageFile / (1024 * 1024)) - *total_mb;
        *swap_used_mb =
            *swap_total_mb -
            (int)((memInfo.ullAvailPageFile - memInfo.ullAvailPhys) /
                  (1024 * 1024));
    } else {
        *total_mb = *used_mb = *free_mb = *avail_mb = 0;
        *buff_mb = *cached_mb = *swap_total_mb = *swap_used_mb = 0;
    }
}

float get_memory_usage_percent(void) {
    MEMORYSTATUSEX memInfo;
    memInfo.dwLength = sizeof(MEMORYSTATUSEX);

    if (GlobalMemoryStatusEx(&memInfo)) {
        return (float)memInfo.dwMemoryLoad / 100.0f;
    }
    return 0.0f;
}

//==============================================================================
// DISK & NETWORK
//==============================================================================

float get_disk_usage(const char *path) {
    ULARGE_INTEGER free, total;
    if (GetDiskFreeSpaceExA(path, &free, &total, NULL)) {
        if (total.QuadPart == 0) return 0.0f;
        return 1.0f - ((float)free.QuadPart / (float)total.QuadPart);
    }
    return 0.0f;
}

void get_disk_space_gb(const char *path, float *total_gb, float *used_gb) {
    ULARGE_INTEGER free_bytes, total_bytes;
    if (GetDiskFreeSpaceExA(path, &free_bytes, &total_bytes, NULL)) {
        *total_gb =
            (float)total_bytes.QuadPart /
            (1024.0f * 1024.0f * 1024.0f);
        float free_gb =
            (float)free_bytes.QuadPart /
            (1024.0f * 1024.0f * 1024.0f);
        *used_gb = *total_gb - free_gb;
    } else {
        *total_gb = 0.0f;
        *used_gb  = 0.0f;
    }
}

void get_disk_io(float *read_mb, float *write_mb) {
    *read_mb = 0.0f;
    *write_mb = 0.0f;
}

void get_network_io(float *rx_mb, float *tx_mb) {
    *rx_mb = 0.0f;
    *tx_mb = 0.0f;
}

//==============================================================================
// SYSTEM INFO
//==============================================================================

long get_uptime_seconds(void) {
    return GetTickCount64() / 1000;
}

void get_load_average(char *buffer, int buf_size) {
    snprintf(buffer, buf_size, "N/A");
}

//==============================================================================
// PROCESS INFORMATION
//==============================================================================

typedef struct {
    int pid;
    char name[256];
    char user[32];
    int state;
    float cpu_percent;
    float mem_percent;
    unsigned long mem_kb;
} process_info_t;

process_info_t* get_process_list(int *count) {
    HANDLE snapshot =
        CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE) {
        *count = 0;
        return NULL;
    }

    PROCESSENTRY32 pe32;
    pe32.dwSize = sizeof(PROCESSENTRY32);
    int proc_count = 0;

    if (Process32First(snapshot, &pe32)) {
        do {
            proc_count++;
        } while (Process32Next(snapshot, &pe32));
    }

    if (proc_count == 0) {
        CloseHandle(snapshot);
        *count = 0;
        return NULL;
    }

    process_info_t *procs =
        (process_info_t*)malloc(sizeof(process_info_t) * proc_count);
    if (!procs) {
        CloseHandle(snapshot);
        *count = 0;
        return NULL;
    }

    pe32.dwSize = sizeof(PROCESSENTRY32);
    int index = 0;

    if (Process32First(snapshot, &pe32)) {
        do {
            procs[index].pid = (int)pe32.th32ProcessID;

            strncpy(procs[index].name, pe32.szExeFile,
                    sizeof(procs[index].name) - 1);
            procs[index].name[sizeof(procs[index].name) - 1] = '\0';

            strcpy(procs[index].user, "user");
            procs[index].state = 1;
            procs[index].cpu_percent = 0.0f;
            procs[index].mem_percent = 0.0f;
            procs[index].mem_kb = 0;

            HANDLE hProc =
                OpenProcess(PROCESS_QUERY_INFORMATION |
                                PROCESS_VM_READ,
                            FALSE,
                            pe32.th32ProcessID);

            if (hProc != NULL) {
                PROCESS_MEMORY_COUNTERS pmc;
                if (GetProcessMemoryInfo(hProc,
                                         &pmc,
                                         sizeof(pmc))) {
                    procs[index].mem_kb =
                        (unsigned long)(pmc.WorkingSetSize / 1024);
                }
                CloseHandle(hProc);
            }

            index++;
        } while (Process32Next(snapshot, &pe32) &&
                 index < proc_count);
    }

    CloseHandle(snapshot);
    *count = index;
    return procs;
}

int kill_process(int pid) {
    HANDLE hProcess =
        OpenProcess(PROCESS_TERMINATE, FALSE, pid);
    if (hProcess == NULL) return 0;

    BOOL result = TerminateProcess(hProcess, 0);
    CloseHandle(hProcess);
    return result ? 1 : 0;
}

void free_process_list(process_info_t *list) {
    if (list) free(list);
}

//==============================================================================
// TERMINAL SIZE
//==============================================================================

int get_terminal_width(void) {
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (h != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(h, &csbi))
        return (int)(csbi.srWindow.Right - csbi.srWindow.Left + 1);
    return 80;
}

int get_terminal_height(void) {
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    if (h != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(h, &csbi))
        return (int)(csbi.srWindow.Bottom - csbi.srWindow.Top + 1);
    return 50;
}