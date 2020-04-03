#include <stdint.h>

#pragma weak cpu_real_time_ticks__=cpu_real_time_ticks
#pragma weak cpu_real_time_ticks_=cpu_real_time_ticks
uint64_t cpu_real_time_ticks__(void);
uint64_t cpu_real_time_ticks_(void);

uint64_t cpu_real_time_ticks(void) {
#if defined(__x86_64__)
  uint32_t lo, hi;   // "in order" version
  __asm__ volatile ("rdtscp"
      : /* outputs   */ "=a" (lo), "=d" (hi)
      : /* no inputs */
      : /* clobbers  */ "%rcx");
  return (uint64_t)lo | (((uint64_t)hi) << 32) ;
#endif
#if defined(__aarch64__)
  asm volatile ("isb; mrs %0, cntvct_el0" : "=r" (time0));
  return time0;
#endif
#if !defined(__x86_64__) && !defined(__aarch64__)
  time0 = cpu_real_time_clock();  // a tick will be a microsecond in this case
  return time0;
#endif
}

