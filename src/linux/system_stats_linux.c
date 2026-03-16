/*******************************************************************************
 * system_stats_linux.c - Linux system statistics (with stubs)
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/sysinfo.h>
#include <sys/statvfs.h>
#include <dirent.h>
#include <ctype.h>
#include <pwd.h>
#include <signal.h>

//==============================================================================
// PLATFORM DETECTION
//==============================================================================

int get_platform(void) {
    return 0;  // Linux
}

//==============================================================================
// CPU STATISTICS
//==============================================================================

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

//==============================================================================
// MEMORY
//==============================================================================

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

    *total_mb    = mem.total_kb / 1024;
    *free_mb     = mem.free_kb / 1024;
    *avail_mb    = mem.available_kb / 1024;
    *buff_mb     = mem.buffers_kb / 1024;
    *cached_mb   = mem.cached_kb / 1024;
    *swap_total_mb = mem.swap_total_kb / 1024;
    *swap_used_mb  = (mem.swap_total_kb - mem.swap_free_kb) / 1024;

    unsigned long used_kb = mem.total_kb - mem.free_kb
                            - mem.buffers_kb - mem.cached_kb;
    *used_mb = used_kb / 1024;
}

//==============================================================================
// DISK & NETWORK
//==============================================================================

float get_disk_usage(const char *path) {
    struct statvfs stat;
    if (statvfs(path, &stat) != 0) return 0.0f;

    unsigned long long total = stat.f_blocks * stat.f_frsize;
    unsigned long long avail = stat.f_bavail * stat.f_frsize;

    if (total == 0) return 0.0f;
    return (float)(total - avail) / (float)total;
}

void get_disk_space_gb(const char *path, float *total_gb, float *used_gb) {
    struct statvfs stat;
    if (statvfs(path, &stat) != 0) {
        *total_gb = 0.0f;
        *used_gb  = 0.0f;
        return;
    }

    unsigned long long total_bytes = (unsigned long long)stat.f_blocks * stat.f_frsize;
    unsigned long long avail_bytes = (unsigned long long)stat.f_bavail * stat.f_frsize;

    *total_gb = (float)total_bytes / (1024.0f * 1024.0f * 1024.0f);
    *used_gb  = (float)(total_bytes - avail_bytes) /
                (1024.0f * 1024.0f * 1024.0f);
}

void get_disk_io(float *read_mb, float *write_mb) {
    // TODO
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
    struct sysinfo info;
    if (sysinfo(&info) != 0) return 0;
    return info.uptime;
}

void get_load_average(char *buffer, int buf_size) {
    FILE *fp = fopen("/proc/loadavg", "r");
    if (!fp) {
        snprintf(buffer, buf_size, "0.00 0.00 0.00");
        return;
    }

    float l1, l5, l15;
    if (fscanf(fp, "%f %f %f", &l1, &l5, &l15) == 3)
        snprintf(buffer, buf_size, "%.2f %.2f %.2f", l1, l5, l15);
    else
        snprintf(buffer, buf_size, "0.00 0.00 0.00");

    fclose(fp);
}

//==============================================================================
// PROCESS STRUCT + STUBS (if needed)
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

/* Stubbed minimal implementation */
process_info_t* get_process_list(int *count) {
    *count = 0;
    return NULL;
}

int kill_process(int pid) {
    return kill(pid, SIGTERM) == 0 ? 1 : 0;
}

void free_process_list(process_info_t *list) {
    if (list) free(list);
}