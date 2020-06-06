#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "HsFFI.h"
#include "plugins-hs.h"

#include "System/Plugins/Export_stub.h"

void potatoInit(void){
  int argc = 2;
  char *argv[] = { (char *)"+RTS", (char *)"-A32m", NULL };
  char **pargv = argv;

  // Initialize Haskell runtime
  hs_init(&argc, &pargv);
}

void potatoExit(void){
  hs_exit();
}
