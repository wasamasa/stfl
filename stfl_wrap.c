#include <locale.h>
#include <stdlib.h>
#include <wchar.h>

struct stfl_ipool *ipool = 0;

void ipool_reset() {
	if (!ipool)
		ipool = stfl_ipool_create("UTF8");
	stfl_ipool_flush(ipool);
}

void ipool_destroy() {
	stfl_ipool_destroy(ipool);
	ipool = 0;
}

#define TOWC(_t) stfl_ipool_towc(ipool, _t)
#define FROMWC(_t) stfl_ipool_fromwc(ipool, _t)

void stfl_init()
{
    setlocale(LC_ALL, "");
}

struct stfl_form *stfl_create_wrapper(const char *text)
{
	ipool_reset();
	return stfl_create(TOWC(text));
}

void stfl_free_wrapper(struct stfl_form *f)
{
	ipool_reset();
	stfl_free(f);
}

const char *stfl_run_wrapper(struct stfl_form *f, int timeout)
{
	ipool_reset();
	return FROMWC(stfl_run(f, timeout));
}

const char *stfl_get_wrapper(struct stfl_form *f, const char *name)
{
	ipool_reset();
	return FROMWC(stfl_get(f, TOWC(name)));
}

void stfl_set_wrapper(struct stfl_form *f, const char *name, const char *value)
{
	ipool_reset();
	return stfl_set(f, TOWC(name), TOWC(value));
}

const char *stfl_get_focus_wrapper(struct stfl_form *f)
{
	ipool_reset();
	return FROMWC(stfl_get_focus(f));
}

void stfl_set_focus_wrapper(struct stfl_form *f, const char *name)
{
	ipool_reset();
	stfl_set_focus(f, TOWC(name));
}

const char *stfl_quote_wrapper(const char *text)
{
	ipool_reset();
	return FROMWC(stfl_quote(TOWC(text)));
}

const char *stfl_dump_wrapper(struct stfl_form *f, const char *name, const char *prefix, int focus)
{
	ipool_reset();
	return FROMWC(stfl_dump(f, TOWC(name), TOWC(prefix), focus));
}

const char *stfl_text_wrapper(struct stfl_form *f, const char *name)
{
	ipool_reset();
	return FROMWC(stfl_text(f, TOWC(name)));
}

void stfl_modify_wrapper(struct stfl_form *f, const char *name, const char *mode, const char *text)
{
	ipool_reset();
	stfl_modify(f, TOWC(name), TOWC(mode), TOWC(text));
}

const char *stfl_error_wrapper()
{
	ipool_reset();
	return FROMWC(stfl_error());
}

void stfl_error_action_wrapper(const char *mode)
{
	ipool_reset();
	stfl_error_action(TOWC(mode));
}
