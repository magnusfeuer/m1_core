//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2004, 2005, 2006, 2007.
//

#ifndef __DB_COMPONENT_H__
#define __DB_COMPONENT_H__

#include "component.hh"
#include "database.hh"

class CDatabaseComponent: public CExecutable {
public:
    XOBJECT_TYPE(CDatabaseComponent,
		 "Database",
		 "Database component",
		 (CDatabaseComponent_format),
		 XFIELD(CDatabaseComponent,Q_PUBLIC,format,
			input_bool_type(),
			"")
	);
public:
    CDatabaseComponent(CExecutor* aExec,
		       CBaseType *aType = CDatabaseComponentType::singleton());
    ~CDatabaseComponent(void);

    void start(CExecutor* aExec);
    void execute(CExecutor* aExec);
    static CDatabase *database(void);
private:
    static CDatabase *mDatabase;
    EventBool mFormat;
};

//
// Persistant bool
//
class CPboolComponent: public CExecutable {
public:
    XOBJECT_TYPE(CPboolComponent, 
		 "Pbool",
		 "Persistant boolean value",
		 (CPboolComponent_value,
		  CPboolComponent_key,
		  CPboolComponent_exists),
		 XFIELD(CPboolComponent,Q_PUBLIC,value,
			event_bool_type(),
			""),
		 XFIELD(CPboolComponent,Q_PUBLIC,key,
			event_string_type(),
			""),
		 XFIELD(CPboolComponent,Q_PUBLIC,exists,
			bool_type(),
			"True if exists in db.")
	);
public:
    CPboolComponent(CExecutor* aExec,
		    CBaseType *aType = CPboolComponentType::singleton());
    ~CPboolComponent(void) {};

    void execute(CExecutor* aExec);
    void start(CExecutor* aExec);
private:
    EventBool mValue;
    EventString mKey;
};


//
// Persistant signed
//
class CPsignedComponent: public CExecutable {
public:
    XOBJECT_TYPE(CPsignedComponent, 
		 "Psigned",
		 "Persistant int value",
		 (CPsignedComponent_value,
		  CPsignedComponent_key,
		  CPsignedComponent_exists),		 
		 XFIELD(CPsignedComponent,Q_PUBLIC,value,
			event_signed_type(),
			""),
		 XFIELD(CPsignedComponent,Q_PUBLIC,key,
			event_string_type(),
			""),
		 XFIELD(CPsignedComponent,Q_PUBLIC,exists,
			bool_type(),
			"True if exists in db.")
	);
public:
    CPsignedComponent(CExecutor* aExec,
		      CBaseType *aType = CPsignedComponentType::singleton());
    ~CPsignedComponent(void) {};

    void execute(CExecutor* aExec);
    void start(CExecutor* aExec);
private:
    EventSigned mValue;
    EventString mKey;
};


//
// Persistant float
//
class CPfloatComponent: public CExecutable {
public:
    XOBJECT_TYPE(CPfloatComponent, 
		 "Pfloat",
		 "Persistant float value",
		 (CPfloatComponent_value,
		  CPfloatComponent_key,
		  CPfloatComponent_exists),
		 XFIELD(CPfloatComponent,Q_PUBLIC,value,
			event_float_type(),
			""),
		 XFIELD(CPfloatComponent,Q_PUBLIC,key,
			event_string_type(),
			""),
		 XFIELD(CPfloatComponent,Q_PUBLIC,exists,
			bool_type(),
			"True if exists in db.")
	);
public:
    CPfloatComponent(CExecutor* aExec,
		     CBaseType *aType = CPfloatComponentType::singleton());
    ~CPfloatComponent(void) {};

    void execute(CExecutor* aExec);
    void start(CExecutor* aExec);
private:
    EventFloat mValue;
    EventString mKey;
};


//
// Persistant string
//
class CPstringComponent: public CExecutable {
public:
    XOBJECT_TYPE(CPstringComponent, 
		"Pstring",
		 "Persistant string value",
		 (CPstringComponent_value,
		  CPstringComponent_key,
		  CPstringComponent_exists),
		 XFIELD(CPstringComponent,Q_PUBLIC,value,
			event_string_type(),
			""),
		 XFIELD(CPstringComponent,Q_PUBLIC,key,
			event_string_type(),
			""),
		 XFIELD(CPstringComponent,Q_PUBLIC,exists,
			bool_type(),
			"True if exists in db.")
	);
public:
    CPstringComponent(CExecutor* aExec,
		      CBaseType *aType = CPstringComponentType::singleton());
    ~CPstringComponent(void) {};

    void execute(CExecutor* aExec);
    void start(CExecutor* aExec);
private:
    EventString mValue;
    EventString mKey;
};

#endif
