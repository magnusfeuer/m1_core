//
//
//
#ifndef __COLOR__HH__
#define __COLOR__HH__

#include "m1.hh"

#define COLOR_A 0
#define COLOR_R 1
#define COLOR_G 2
#define COLOR_B 3

class CColor {  // Dummy
public:
    class CColorType : public CBaseType {
    public:
	static CColorType* singleton(void) {
	    static CColorType *result = 0;
	    if (!result) result = m1New(CColorType);
	    return result;
	}

	CColorType() : CBaseType("Color") {
	    CField field_desc[] = {
		CField(Q_PUBLIC, "r", byte_type()),
		CField(Q_PUBLIC, "g", byte_type()),
		CField(Q_PUBLIC, "b", byte_type()),
		CField(Q_PUBLIC, "value", unsigned_type()) };
	    setupFields(field_desc,sizeof(field_desc)/sizeof(field_desc[0]),0);
	}

	UData produce(CExecutor* aExec,CBaseType* aBase, CArgs* args) {
	    return UUnsigned(0);
	}

	UData produceEvent(CExecutor* aExec, bool aQueued);

	UData  retainObject(UData color)      { return color; }
	UData  releaseObject(UData color)     { return color; }

	void shallowCopy(CExecutor* aExec, UData src, UData* dst) { *dst = src; }
	void deepCopy(CExecutor* aExec, UData src, UData* dst)    { *dst = src; }

	const size_t sizeOf(void) { return sizeof(UData); }

	const M1TypeTag typeTag(void)      { return M1TYPE_UNSIGNED; }

	int compare(UData a, UData b) {
	    if (a.u == b.u) return 0;
	    else if (a.u < b.u) return -1;
	    return 1;
	}

	void elementInit(CExecutor* aExec,UDataVector *vec,int index) { 
	    vec->at(index) = nil;
	}

	UData elementAt(UDataVector *vec,int index) {
	    UData r;
	    r.ud = &vec->at(index);
	    return r;
	}

	void elementPut(CExecutor* aExec,UDataVector *vec,int index,UData value, bool trig=false) {
	    vec->at(index) = *value.ud;
	}

	// void print(ostream* os, UData a);

	void init(CExecutor*, CBaseObject*) {}
	void defaults(CExecutor*, CBaseObject*) {}
	void construct(CExecutor*, CBaseObject*) {}
	void destruct(CExecutor*, CBaseObject*) {}
    };
public:
    //
private:
    //
};

#endif
