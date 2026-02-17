/*******************************************************************************
 * crossplatform_system_stats.c - Cross-platform system statistics
 * Supports both Linux and Windows
 ******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
    #define IS_WINDOWS 1
    #include <windows.h>
    #include <psapi.h>
    #include <tlhelp32.h>
    #pragma comment(lib, "psapi.lib")
#else
    #define IS_WINDOWS 0
    #include <unistd.h>
    #include <sys/sysinfo.h>
    #include <sys/statvfs.h>
    #include <dirent.h>
    #include <ctype.h>
    #include <pwd.h>
    #include <signal.h>
#endif

//==============================================================================
// PLATFORM DETECTION
//==============================================================================

int get_platform(void) {
    return IS_WINDOWS ? 1 : 0;  // 0 = Linux, 1 = Windows
}

//==============================================================================
// CPU STATISTICS
//==============================================================================

#if IS_WINDOWS

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
    
    // On Windows, we get overall CPU, not per-core easily
    // For now, return same value for all cores
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
        
        ULONGLONG idle_diff = ((ULONGLONG)idle.dwHighDateTime << 32 | idle.dwLowDateTime) -
                              ((ULONGLONG)prev_idle[0].dwHighDateTime << 32 | prev_idle[0].dwLowDateTime);
        ULONGLONG kernel_diff = ((ULONGLONG)kernel.dwHighDateTime << 32 | kernel.dwLowDateTime) -
                                ((ULONGLONG)prev_kernel[0].dwHighDateTime << 32 | prev_kernel[0].dwLowDateTime);
        ULONGLONG user_diff = ((ULONGLONG)user.dwHighDateTime << 32 | user.dwLowDateTime) -
                              ((ULONGLONG)prev_user[0].dwHighDateTime << 32 | prev_user[0].dwLowDateTime);
        
        ULONGLONG total = kernel_diff + user_diff;
        float cpu_percent = total > 0 ? (float)(total - idle_diff) / total : 0.0f;
        
        for (int i = 0; i < num_cores; i++) usage[i] = cpu_percent;
        
        prev_idle[0] = idle;
        prev_kernel[0] = kernel;
        prev_user[0] = user;
        
        return num_cores;
    }
    
    for (int i = 0; i < num_cores; i++) usage[i] = 0.0f;
    return num_cores;
}

#else  // Linux

typedef struct {
    unsigned long long user, nice, system, idle, iowait, irq, softirq;
} cpu_stat_t;

static cpu_stat_t prev_stats[128];
static int num_cores = 0;
static int initialized = 0;

static int read_cpu_stats(cpu_stat_t stats[], int max) {
    FILE *fp = fopen("/proc/stat", "r");
    if (!fp) return -1;
    
    char line[256];
    int count = 0;
    
    while (fgets(line, sizeof(line), fp) && count < max) {
        if (strncmp(line, "cpu", 3) == 0 && line[3] >= '0' && line[3] <= '9') {
            sscanf(line, "cpu%*d %llu %llu %llu %llu %llu %llu %llu",
                   &stats[count].user, &stats[count].nice, &stats[count].system,
                   &stats[count].idle, &stats[count].iowait, &stats[count].irq,
                   &stats[count].softirq);
            count++;
        }
    }
    fclose(fp);
    return count;
}

static float calc_usage(cpu_stat_t *p, cpu_stat_t *c) {
    unsigned long long p_idle = p->idle + p->iowait;
    unsigned long long c_idle = c->idle + c->iowait;
    unsigned long long p_total = p->user + p->nice + p->system + p_idle + p->irq + p->softirq;
    unsigned long long c_total = c->user + c->nice + c->system + c_idle + c->irq + c->softirq;
    
    unsigned long long total_diff = c_total - p_total;
    unsigned long long idle_diff = c_idle - p_idle;
    
    if (total_diff == 0) return 0.0f;
    return (float)(total_diff - idle_diff) / (float)total_diff;
}

int get_num_cpu_cores(void) {
    return sysconf(_SC_NPROCESSORS_ONLN);
}

int get_cpu_usage(float usage[], int max) {
    cpu_stat_t curr[128];
    int cores = read_cpu_stats(curr, max);
    if (cores <= 0) return 0;
    
    if (!initialized) {
        memcpy(prev_stats, curr, sizeof(cpu_stat_t) * cores);
        num_cores = cores;
        initialized = 1;
        for (int i = 0; i < cores; i++) usage[i] = 0.0f;
        return cores;
    }
    
    for (int i = 0; i < cores && i < num_cores; i++)
        usage[i] = calc_usage(&prev_stats[i], &curr[i]);
    
    memcpy(prev_stats, curr, sizeof(cpu_stat_t) * cores);
    num_cores = cores;
    return cores;
}

#endif

//==============================================================================
// MEMORY STATISTICS
//==============================================================================

#if IS_WINDOWS

void get_memory_detailed(int *total_mb, int *used_mb, int *free_mb, 
                         int *avail_mb, int *buff_mb, int *cached_mb,
                         int *swap_total_mb, int *swap_used_mb) {
    MEMORYSTATUSEX memInfo;
    memInfo.dwLength = sizeof(MEMORYSTATUSEX);
    
    if (GlobalMemoryStatusEx(&memInfo)) {
        *total_mb = (int)(memInfo.ullTotalPhys / (1024 * 1024));
        *avail_mb = (int)(memInfo.ullAvailPhys / (1024 * 1024));
        *used_mb = *total_mb - *avail_mb;
        *free_mb = *avail_mb;
        *buff_mb = 0;  // Not available on Windows
        *cached_mb = 0;
        *swap_total_mb = (int)(memInfo.ullTotalPageFile / (1024 * 1024)) - *total_mb;
        *swap_used_mb = *swap_total_mb - (int)((memInfo.ullAvailPageFile - memInfo.ullAvailPhys) / (1024 * 1024));
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

#else  // Linux

typedef struct {
    unsigned long total_kb, free_kb, available_kb, buffers_kb, cached_kb;
    unsigned long swap_total_kb, swap_free_kb;
} memory_stat_t;

static int get_memory_stats(memory_stat_t *mem) {
    FILE *fp = fopen("/proc/meminfo", "r");
    if (!fp) return -1;
    
    char line[256];
    memset(mem, 0, sizeof(memory_stat_t));
    
    while (fgets(line, sizeof(line), fp)) {
        sscanf(line, "MemTotal: %lu kB", &mem->total_kb);
        sscanf(line, "MemFree: %lu kB", &mem->free_kb);
        sscanf(line, "MemAvailable: %lu kB", &mem->available_kb);
        sscanf(line, "Buffers: %lu kB", &mem->buffers_kb);
        sscanf(line, "Cached: %lu kB", &mem->cached_kb);
        sscanf(line, "SwapTotal: %lu kB", &mem->swap_total_kb);
        sscanf(line, "SwapFree: %lu kB", &mem->swap_free_kb);
    }
    fclose(fp);
    return 0;
}

float get_memory_usage_percent(void) {
    memory_stat_t mem;
    if (get_memory_stats(&mem) != 0) return 0.0f;
    if (mem.total_kb == 0) return 0.0f;
    
    unsigned long used = mem.total_kb - mem.available_kb;
    return (float)used / (float)mem.total_kb;
}

void get_memory_detailed(int *total_mb, int *used_mb, int *free_mb, 
                         int *avail_mb, int *buff_mb, int *cached_mb,
                         int *swap_total_mb, int *swap_used_mb) {
    memory_stat_t mem;
    if (get_memory_stats(&mem) != 0) {
        *total_mb = *used_mb = *free_mb = *avail_mb = 0;
        *buff_mb = *cached_mb = *swap_total_mb = *swap_used_mb = 0;
        return;
    }
    
    *total_mb = mem.total_kb / 1024;
    *free_mb = mem.free_kb / 1024;
    *avail_mb = mem.available_kb / 1024;
    *buff_mb = mem.buffers_kb / 1024;
    *cached_mb = mem.cached_kb / 1024;
    *swap_total_mb = mem.swap_total_kb / 1024;
    *swap_used_mb = (mem.swap_total_kb - mem.swap_free_kb) / 1024;
    *used_mb = *total_mb - *avail_mb;
}

#endif

//==============================================================================
// DISK & NETWORK (Simplified for cross-platform)
//==============================================================================

float get_disk_usage(const char *path) {
#if IS_WINDOWS
    ULARGE_INTEGER free, total;
    if (GetDiskFreeSpaceExA(path, &free, &total, NULL)) {
        if (total.QuadPart == 0) return 0.0f;
        return 1.0f - ((float)free.QuadPart / (float)total.QuadPart);
    }
    return 0.0f;
#else
    struct statvfs stat;
    if (statvfs(path, &stat) != 0) return 0.0f;
    
    unsigned long long total = stat.f_blocks * stat.f_frsize;
    unsigned long long avail = stat.f_bavail * stat.f_frsize;
    
    if (total == 0) return 0.0f;
    return (float)(total - avail) / (float)total;
#endif
}

void get_disk_io(float *read_mb, float *write_mb) {
    // Simplified - just return 0 for now on both platforms
    // Full implementation would require platform-specific APIs
    *read_mb = 0.0f;
    *write_mb = 0.0f;
}

void get_network_io(float *rx_mb, float *tx_mb) {
    // Simplified - return 0 for now
    *rx_mb = 0.0f;
    *tx_mb = 0.0f;
}

//==============================================================================
// SYSTEM INFO
//==============================================================================

long get_uptime_seconds(void) {
#if IS_WINDOWS
    return GetTickCount64() / 1000;
#else
    struct sysinfo info;
    if (sysinfo(&info) != 0) return 0;
    return info.uptime;
#endif
}

void get_load_average(char *buffer, int buf_size) {
#if IS_WINDOWS
    snprintf(buffer, buf_size, "N/A");
#else
    FILE *fp = fopen("/proc/loadavg", "r");
    if (!fp) {
        snprintf(buffer, buf_size, "0.00 0.00 0.00");
        return;
    }
    
    float load1, load5, load15;
    if (fscanf(fp, "%f %f %f", &load1, &load5, &load15) == 3) {
        snprintf(buffer, buf_size, "%.2f %.2f %.2f", load1, load5, load15);
    } else {
        snprintf(buffer, buf_size, "0.00 0.00 0.00");
    }
    fclose(fp);
#endif
}

//==============================================================================
// PROCESS INFORMATION
//==============================================================================

typedef struct {
    int pid;
    char name[256];
    char user[32];
    int state;  // 0=Running, 1=Sleeping, 2=Stopped, 3=Zombie, 4=Unknown
    float cpu_percent;
    float mem_percent;
    unsigned long mem_kb;
} process_info_t;

#if IS_WINDOWS

process_info_t* get_process_list(int *count) {
    HANDLE snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snapshot == INVALID_HANDLE_VALUE) {
        *count = 0;
        return NULL;
    }
    
    // Count processes
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
    
    process_info_t *procs = (process_info_t*)malloc(sizeof(process_info_t) * proc_count);
    if (!procs) {
        CloseHandle(snapshot);
        *count = 0;
        return NULL;
    }
    
    // Get process info
    pe32.dwSize = sizeof(PROCESSENTRY32);
    int index = 0;
    
    if (Process32First(snapshot, &pe32)) {
        do {
            procs[index].pid = pe32.th32ProcessID;
            WideCharToMultiByte(CP_UTF8, 0, pe32.szExeFile, -1, 
                                procs[index].name, sizeof(procs[index].name), NULL, NULL);
            strcpy(procs[index].user, "user");  // Simplified
            procs[index].state = 1;  // Sleeping (default)
            procs[index].cpu_percent = 0.0f;  // Would need sampling
            procs[index].mem_percent = 0.0f;
            procs[index].mem_kb = pe32.th32MemoryUsage / 1024;
            index++;
        } while (Process32Next(snapshot, &pe32) && index < proc_count);
    }
    
    CloseHandle(snapshot);
    *count = index;
    return procs;
}

int kill_process(int pid) {
    HANDLE hProcess = OpenProcess(PROCESS_TERMINATE, FALSE, pid);
    if (hProcess == NULL) return 0;
    
    BOOL result = TerminateProcess(hProcess, 0);
    CloseHandle(hProcess);
    return result ? 1 : 0;
}

#else  // Linux

static int is_number(const char *str) {
    while (*str) {
        if (!isdigit(*str)) return 0;
        str++;
    }
    return 1;
}

static void get_process_info(int pid, process_info_t *info) {
    char path[256];
    FILE *fp;
    
    memset(info, 0, sizeof(process_info_t));
    info->pid = pid;
    
    snprintf(path, sizeof(path), "/proc/%d/stat", pid);
    fp = fopen(path, "r");
    if (fp) {
        unsigned long utime, stime;
        long rss;
        char state;
        
        fscanf(fp, "%*d (%255[^)]) %c %*d %*d %*d %*d %*d %*u %*u %*u %*u %*u %lu %lu %*d %*d %*d %*d %*d %*d %*u %*u %ld",
               info->name, &state, &utime, &stime, &rss);
        
        info->mem_kb = rss * (sysconf(_SC_PAGESIZE) / 1024);
        
        // Map state character to enum
        switch (state) {
            case 'R': info->state = 0; break;  // Running
            case 'S': case 'D': info->state = 1; break;  // Sleeping
            case 'T': info->state = 2; break;  // Stopped
            case 'Z': info->state = 3; break;  // Zombie
            default: info->state = 4; break;   // Unknown
        }
        
        long hz = sysconf(_SC_CLK_TCK);
        unsigned long total_time = utime + stime;
        info->cpu_percent = (float)total_time / hz / get_uptime_seconds() * 100.0f;
        
        fclose(fp);
    }
    
    memory_stat_t mem;
    if (get_memory_stats(&mem) == 0 && mem.total_kb > 0) {
        info->mem_percent = (float)info->mem_kb / (float)mem.total_kb * 100.0f;
    }
    
    snprintf(path, sizeof(path), "/proc/%d/status", pid);
    fp = fopen(path, "r");
    if (fp) {
        char line[256];
        int uid = 0;
        
        while (fgets(line, sizeof(line), fp)) {
            if (sscanf(line, "Uid:\t%d", &uid) == 1) {
                struct passwd *pw = getpwuid(uid);
                if (pw) {
                    strncpy(info->user, pw->pw_name, sizeof(info->user) - 1);
                } else {
                    snprintf(info->user, sizeof(info->user), "%d", uid);
                }
                break;
            }
        }
        fclose(fp);
    }
}

process_info_t* get_process_list(int *count) {
    DIR *dir = opendir("/proc");
    if (!dir) {
        *count = 0;
        return NULL;
    }
    
    // Count processes
    struct dirent *entry;
    int max_procs = 0;
    while ((entry = readdir(dir)) != NULL) {
        if (is_number(entry->d_name)) max_procs++;
    }
    
    if (max_procs == 0) {
        closedir(dir);
        *count = 0;
        return NULL;
    }
    
    process_info_t *procs = (process_info_t*)malloc(sizeof(process_info_t) * max_procs);
    if (!procs) {
        closedir(dir);
        *count = 0;
        return NULL;
    }
    
    rewinddir(dir);
    int index = 0;
    
    while ((entry = readdir(dir)) != NULL && index < max_procs) {
        if (is_number(entry->d_name)) {
            int pid = atoi(entry->d_name);
            get_process_info(pid, &procs[index]);
            if (procs[index].pid > 0) index++;
        }
    }
    closedir(dir);
    
    *count = index;
    
    // Sort by CPU
    for (int i = 0; i < index - 1; i++) {
        for (int j = 0; j < index - i - 1; j++) {
            if (procs[j].cpu_percent < procs[j + 1].cpu_percent) {
                process_info_t temp = procs[j];
                procs[j] = procs[j + 1];
                procs[j + 1] = temp;
            }
        }
    }
    
    return procs;
}

int kill_process(int pid) {
    return kill(pid, SIGTERM) == 0 ? 1 : 0;
}

#endif

void free_process_list(process_info_t *list) {
    if (list) free(list);
}