/* straggler — a synthetic fork-join workload for vdb's sched pane.
 *
 * N workers run rounds of CPU work that end at a barrier, so every round the whole
 * pool waits for its slowest thread. One worker is the straggler, and -m picks why
 * it lags — the two causes that look identical in wall-clock timings but render
 * differently on the scheduling timeline:
 *
 *   -m skew     the straggler gets -k times more work per round. It lags because
 *               it is busy: its lane stays █ (running) long after the others have
 *               gone · (sleeping at the barrier).
 *
 *   -m starve   every worker gets the same work, but the straggler is pinned to a
 *               CPU it shares with two spinning antagonist threads. It lags because
 *               it can't get scheduled: its lane shows ▓ (runnable, waiting for
 *               CPU) instead of █.
 *
 * Usage:
 *   ./straggler [-m skew|starve] [-t threads] [-w work_ms] [-k skew] [-r rounds]
 *               [-p pause_ms] [-c cpu] [-R]
 *
 *   -t  worker threads                    (default 8)
 *   -w  CPU ms each worker burns per round (default 2000; that's 4 cells at vdb's
 *       default 500ms sampling)
 *   -k  straggler work multiplier, skew mode (default 4)
 *   -r  rounds to run, 0 = until killed   (default 0)
 *   -p  pause between rounds, ms          (default 1000)
 *   -c  the contended CPU, starve mode    (default 0)
 *   -R  rotate the straggler each round, skew mode (a moving straggler draws a
 *       diagonal across the swimlanes)
 *
 * It prints its pid on startup: open a sched pane in vdb, press p, type the pid,
 * Enter. Build with `make bench` (or: gcc -O2 -pthread -o straggler straggler.c).
 */

#define _GNU_SOURCE
#include <pthread.h>
#include <sched.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

static int nthreads = 8;
static long work_ms = 2000;
static int skew = 4;
static long rounds = 0;
static long pause_ms = 1000;
static int contended_cpu = 0;
static int rotate = 0;
static enum { MODE_SKEW, MODE_STARVE } mode = MODE_SKEW;

static pthread_barrier_t barrier;
static _Atomic int last_finisher;

static double now_s(void)
{
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec + ts.tv_nsec / 1e9;
}

static long thread_cpu_ms(void)
{
  struct timespec ts;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &ts);
  return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

/* Burn [ms] of this thread's *CPU* time, not wall time: a starved thread needs the
 * same CPU ms but far more wall clock to collect them, which is the whole point. */
static void burn_cpu_ms(long ms)
{
  long deadline = thread_cpu_ms() + ms;
  volatile unsigned long sink = 0;
  while (thread_cpu_ms() < deadline)
    for (int i = 0; i < 100000; i++)
      sink += i;
}

static void pin_to_contended_cpu(void)
{
  cpu_set_t set;
  CPU_ZERO(&set);
  CPU_SET(contended_cpu, &set);
  if (pthread_setaffinity_np(pthread_self(), sizeof set, &set) != 0)
    fprintf(stderr, "warning: could not pin to cpu %d\n", contended_cpu);
}

static void sleep_ms(long ms)
{
  struct timespec ts = { .tv_sec = ms / 1000, .tv_nsec = (ms % 1000) * 1000000 };
  nanosleep(&ts, NULL);
}

static int straggler_of_round(long round)
{
  return rotate && mode == MODE_SKEW ? (int)(round % nthreads) : nthreads - 1;
}

struct worker
{
  int id;
  pthread_t thread;
};

static void *worker_main(void *arg)
{
  struct worker *w = arg;
  char name[16];
  snprintf(name, sizeof name, "worker-%d", w->id);
  pthread_setname_np(pthread_self(), name);
  if (mode == MODE_STARVE && w->id == nthreads - 1)
    pin_to_contended_cpu();
  for (long round = 0; rounds == 0 || round < rounds; round++) {
    long ms = work_ms;
    if (mode == MODE_SKEW && w->id == straggler_of_round(round))
      ms *= skew;
    burn_cpu_ms(ms);
    atomic_store(&last_finisher, w->id);
    pthread_barrier_wait(&barrier);
    if (pause_ms)
      sleep_ms(pause_ms);
  }
  return NULL;
}

static void *antagonist_main(void *arg)
{
  (void)arg;
  pthread_setname_np(pthread_self(), "antagonist");
  pin_to_contended_cpu();
  volatile unsigned long sink = 0;
  for (;;)
    sink += 1;
  return NULL;
}

int main(int argc, char **argv)
{
  int opt;
  while ((opt = getopt(argc, argv, "t:w:k:r:p:c:m:Rh")) != -1) {
    switch (opt) {
    case 't': nthreads = atoi(optarg); break;
    case 'w': work_ms = atol(optarg); break;
    case 'k': skew = atoi(optarg); break;
    case 'r': rounds = atol(optarg); break;
    case 'p': pause_ms = atol(optarg); break;
    case 'c': contended_cpu = atoi(optarg); break;
    case 'R': rotate = 1; break;
    case 'm':
      if (strcmp(optarg, "skew") == 0)
        mode = MODE_SKEW;
      else if (strcmp(optarg, "starve") == 0)
        mode = MODE_STARVE;
      else {
        fprintf(stderr, "unknown mode %s (want skew|starve)\n", optarg);
        return 2;
      }
      break;
    default:
      fprintf(stderr,
              "usage: %s [-m skew|starve] [-t threads] [-w work_ms] [-k skew]"
              " [-r rounds] [-p pause_ms] [-c cpu] [-R]\n",
              argv[0]);
      return opt == 'h' ? 0 : 2;
    }
  }
  if (nthreads < 2 || work_ms <= 0 || skew < 1) {
    fprintf(stderr, "need -t >= 2, -w > 0, -k >= 1\n");
    return 2;
  }

  printf("straggler: pid %d — in vdb's sched pane press p, type %d, Enter\n",
         getpid(), getpid());
  printf("mode %s · %d workers · %ldms cpu/round%s · straggler %s\n",
         mode == MODE_SKEW ? "skew" : "starve", nthreads, work_ms,
         mode == MODE_SKEW ? "" : " each",
         mode == MODE_STARVE  ? "pinned with 2 antagonists"
         : rotate             ? "rotates each round"
                              : "is the last worker");
  fflush(stdout);

  /* Workers + main all meet at the barrier, so main can time each round. */
  pthread_barrier_init(&barrier, NULL, nthreads + 1);

  if (mode == MODE_STARVE)
    for (int i = 0; i < 2; i++) {
      pthread_t t;
      pthread_create(&t, NULL, antagonist_main, NULL);
    }

  struct worker *workers = calloc(nthreads, sizeof *workers);
  for (int i = 0; i < nthreads; i++) {
    workers[i].id = i;
    pthread_create(&workers[i].thread, NULL, worker_main, &workers[i]);
  }

  double start = now_s();
  for (long round = 0; rounds == 0 || round < rounds; round++) {
    double round_start = now_s();
    pthread_barrier_wait(&barrier);
    int last = atomic_load(&last_finisher);
    printf("round %ld: %.1fs wall, last to finish: worker-%d%s\n", round,
           now_s() - round_start, last,
           last == straggler_of_round(round) ? " (the straggler)" : "");
    fflush(stdout);
    if (pause_ms)
      sleep_ms(pause_ms);
  }

  for (int i = 0; i < nthreads; i++)
    pthread_join(workers[i].thread, NULL);
  printf("total: %.1fs wall for %ld rounds\n", now_s() - start, rounds);
  return 0;
}
