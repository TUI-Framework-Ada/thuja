/*******************************************************************************
 * system_stats.c - Linux system statistics (single file)
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/sysinfo.h>
#include <sys/statvfs.h>

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

int get_num_cpu_cores(void) {
    return sysconf(_SC_NPROCESSORS_ONLN);
}

typedef struct {
    unsigned long total_kb, free_kb, available_kb, buffers_kb, cached_kb;
} memory_stat_t;

int get_memory_stats(memory_stat_t *mem) {
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

float get_disk_usage(const char *path) {
    struct statvfs stat;
    if (statvfs(path, &stat) != 0) return 0.0f;
    unsigned long long total = stat.f_blocks * stat.f_frsize;
    unsigned long long avail = stat.f_bavail * stat.f_frsize;
    unsigned long long used = total - avail;
    if (total == 0) return 0.0f;
    return (float)used / (float)total;
}

long get_uptime_seconds(void) {
    struct sysinfo info;
    if (sysinfo(&info) != 0) return 0;
    return info.uptime;
}