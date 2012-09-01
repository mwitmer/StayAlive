#include <libguile.h>

static void scm_main(void *data, int argc, char ** argv) {
  scm_shell(argc, argv);
}

int main(int argc, char ** argv) {
  char scm_file[512];
  sprintf(scm_file, "%s/stay_alive.scm", SCMDIR);
  char * my_argv[]  = {argv[0],  "-s", scm_file};
  scm_boot_guile(3, my_argv, scm_main, NULL);
  return 0;
}
