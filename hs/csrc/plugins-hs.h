#pragma once

#ifdef __cplusplus
extern "C" {
#endif

void potatoInit(void);

void potatoExit(void);

void callback_c(void (*f)(void*), void* x);
#ifdef __cplusplus
}
#endif
