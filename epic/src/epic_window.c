/*
 * Window managent
 *
 */

#include "epic.h"

void EWINDOW_TYPE_RELEASE(void* arg)
{
    EWindow* win = (EWindow*) arg;

    EDBGFMT_MEM("EWINDOW_TYPE_RELEASE: %p", arg);
    EWindowDetach(win);
    if (win->on_heap)
	free(win);
}


EWindow* EWindowCreate(int x, int y, unsigned int width, unsigned int height)
{
    EWindow* win;

    if ((win = (EWindow*) malloc(sizeof(EWindow))) == NULL)
	return NULL;
    EOBJECT_INIT(win, EWINDOW_TYPE);
    win->on_heap = 1;
    win->refc = 1;

    win->backend = NULL;
    win->mask    = 0;
    win->x = x;
    win->y = y;
    win->width = width;
    win->height = height;
    return win;
}

int EWindowSwap(EWindow* win)
{
    if (win->backend == NULL)
	return -1;
    return EBackendWindowSwap(win->backend, win);
}

int EWindowAttach(EWindow* win, EBackend* be)
{
    return EBackendWindowAttach(be, win);
}

int EWindowDetach(EWindow* win)
{
    if (win->backend == NULL)
	return -1;
    return EBackendWindowDetach(win->backend, win);
}


