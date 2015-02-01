//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2006, 2007.
//
#include "db_component.hh"

using namespace std;

XOBJECT_TYPE_BOOTSTRAP(CDatabaseComponent);
XOBJECT_TYPE_BOOTSTRAP(CPboolComponent);
XOBJECT_TYPE_BOOTSTRAP(CPsignedComponent);
XOBJECT_TYPE_BOOTSTRAP(CPfloatComponent);
XOBJECT_TYPE_BOOTSTRAP(CPstringComponent);
CDatabase *CDatabaseComponent::mDatabase = 0;

CDatabaseComponent::CDatabaseComponent(CExecutor* aExec,CBaseType *aType) :
    CExecutable(aExec, aType),
    mFormat(this)
{
    eventPut(aExec,XINDEX(CDatabaseComponent,format), &mFormat);
}

CDatabaseComponent::~CDatabaseComponent(void)
{
    if (mDatabase) 
	mDatabase->flush();
}


void CDatabaseComponent::start(CExecutor* aExec)
{
    // Reformat the database?
}


void CDatabaseComponent::execute(CExecutor* aExec)
{
    if (mFormat.updated() && mDatabase) {
	DBGFMT("CDatabaseComponent::execute(): Will format database [%s].", getenv("M1_DATABASE"));
	mDatabase->format();
    }
}

CDatabase *CDatabaseComponent::database(void) 
{
    if (!mDatabase) {
	char *db;
	if ((db = getenv("M1_DATABASE"))) {
	    mDatabase = new CDatabase;
	    mDatabase->blocks(128); // For now.
	    mDatabase->blockSize(4096); // For now.
	    mDatabase->device(db);
	    if (!mDatabase->load()) {
		DBGFMT("CDatabaseComponent::databaset(): WARNING. Could not read database from [%s]. Will reformat", 
		       db);
		if (!mDatabase->format()) {
		    WARNFMT("CDatabaseComponent::database(): Could not format database at[%s].", 
			   db);
		    delete mDatabase;
		    mDatabase = 0;
		}
	    }
	    else
		DBGFMT("CDatabaseComponent::database(): Will use DB [%s]. ", db);
	} else {
	    static bool have_bitcehd = false;

	    if (!have_bitcehd) {
		DBGFMT("CDatabaseComponent::database(): No environment variable M1_DATABASE defined. No database available. ");
		have_bitcehd = true;
	    }
	}
    }
    return mDatabase;
}

CPboolComponent::CPboolComponent(CExecutor* aExec, CBaseType *aType):
    CExecutable(aExec, aType),
    mValue(this),
    mKey(this)
{
    mValue.putValue(aExec, false);
    put(aExec, XINDEX(CPboolComponent,exists), UFalse());
    mKey.putValue(aExec, "");
    eventPut(aExec,XINDEX(CPboolComponent,value), &mValue);
    eventPut(aExec,XINDEX(CPboolComponent,key), &mKey);
}

void CPboolComponent::start(CExecutor* aExec)
{
    DBGFMT("CPboolComponent::start(): Called. Key[%s][%d]", 
	   mKey.value().c_str(), mKey.updated());
    mValue.cancel(aExec);
    mKey.setUpdated(aExec);
    execute(aExec);
}

void CPboolComponent::execute(CExecutor* aExec) 
{
    CDatabase *db = CDatabaseComponent::database();
    if (!db)
	return;

    // 
    // If key has been updated, we need to retrieve self from the database, if possible
    //
    if (mKey.updated()) {
	int tmp;

	if (db->get(mKey.value(), &tmp)) {
	    // Don't override newly updated value.
	    if (!mValue.updated()) {
		put(aExec,XINDEX(CPboolComponent,exists),UTrue());
		mValue.putValue(aExec, tmp); // We'll have to do this top activate the assign operator.
		DBGFMT("CPboolComponent::execute(): Retrieved key[%s] Value[%d]", mKey.value().c_str(), mValue.value());
	    } else
		DBGFMT("CPboolComponent::execute(): key[%s] is updated (Value[%d]). Will not override", mKey.value().c_str(), mValue.value());
		
	} else {
	    DBGFMT("CPboolComponent::execute(): Not found key[%s]", mKey.value().c_str());
	    put(aExec,XINDEX(CPboolComponent,exists),UFalse());
	}
    }

    //
    // Store self.
    //
    if (mValue.updated() && mKey.value() != "") {
	put(aExec,XINDEX(CPboolComponent,exists),UTrue()); // It will exist after this one.
	if (db->set(mKey.value(), (int) mValue.value()))
	    DBGFMT("CPboolComponent::execute(): Stored  key[%s] value[%d]", mKey.value().c_str(), (int) mValue.value());
	else
	    DBGFMT("CPboolComponent::execute(): No need to store key[%s] value[%d], value not updated.", mKey.value().c_str(), (int) mValue.value());
	return;
    }	
}


CPsignedComponent::CPsignedComponent(CExecutor* aExec, CBaseType *aType):
    CExecutable(aExec, aType),
    mValue(this),
    mKey(this)
{
    mValue.putValue(aExec, 0);
    put(aExec, XINDEX(CPboolComponent,exists), UFalse());
    mKey.putValue(aExec, "");
    eventPut(aExec,XINDEX(CPsignedComponent,value), &mValue);
    eventPut(aExec,XINDEX(CPsignedComponent,key), &mKey);
}

void CPsignedComponent::start(CExecutor* aExec) 
{
    DBGFMT("CPsignedComponent::start(): Called. Key[%s][%d]", 
	   mKey.value().c_str(), mKey.updated());
    mValue.cancel(aExec);
    mKey.setUpdated(aExec);
    execute(aExec);
}

void CPsignedComponent::execute(CExecutor* aExec) 
{
    CDatabase *db = CDatabaseComponent::database();
    if (!db)
	return;

    // 
    // If key has been updated, we need to retrieve self from the database, if possible
    //
    if (mKey.updated()) {
	int tmp;

	// Flush new value if updated.
	if (db->get(mKey.value(), &tmp)) {
	    // Don't override newly updated value.
	    if (!mValue.updated()) {
		put(aExec,XINDEX(CPsignedComponent,exists), UTrue());
		mValue.putValue(aExec, tmp); // We'll have to do this top activate the assign operator.
		DBGFMT("CPsignedComponent::execute(): Retrieved key[%s] Value[%d]\n", mKey.value().c_str(), mValue.value());
	    } else
		DBGFMT("CPsignedComponent::execute(): Value updated for key[%s]. Not retrieved.\n", mKey.value().c_str());
	} else {
	    DBGFMT("CPsignedComponent::execute(): Not found key[%s]", mKey.value().c_str());
	    put(aExec,XINDEX(CPsignedComponent,exists), UFalse());
	}
    }

    //
    // Store self.
    //
    if (mValue.updated() && mKey.value() != "") {
	put(aExec,XINDEX(CPsignedComponent,exists), UTrue()); // It will exist after this one.
	if (db->set(mKey.value(), mValue.value())) 
	    DBGFMT("CPsignedComponent::execute(): Stored  key[%s] value[%d]\n", mKey.value().c_str(), mValue.value());
	else
	    DBGFMT("CPsignedComponent::execute(): No need to store key[%s] value[%d], value not updated.\n", mKey.value().c_str(), mValue.value());
	return;
    }	
}

CPfloatComponent::CPfloatComponent(CExecutor* aExec, CBaseType *aType):
    CExecutable(aExec, aType),
    mValue(this),
    mKey(this)
{
    mValue.putValue(aExec, 0.0);
    put(aExec, XINDEX(CPfloatComponent,exists), UFalse());
    mKey.putValue(aExec, "");
    eventPut(aExec,XINDEX(CPfloatComponent,value), &mValue);
    eventPut(aExec,XINDEX(CPfloatComponent,key), &mKey);
}

void CPfloatComponent::start(CExecutor* aExec) 
{
    DBGFMT("CPfloatComponent::start(): Called. Key[%s][%d]",
	   mKey.value().c_str(), mKey.updated());
    mValue.cancel(aExec);
    mKey.setUpdated(aExec);
    execute(aExec);
}

void CPfloatComponent::execute(CExecutor* aExec) 
{
    CDatabase *db = CDatabaseComponent::database();
    if (!db)
	return;

    // 
    // If key has been updated, we need to retrieve self from the database, if possible
    //
    if (mKey.updated()) {
	float tmp;

	if (db->get(mKey.value(), &tmp)) {
	    // Don't override newly updated value.
	    if (!mValue.updated()) {
		put(aExec,XINDEX(CPfloatComponent,exists), UTrue());
		mValue.putValue(aExec, tmp); // We'll have to do this top activate the assign operator.
		DBGFMT("CPfloatComponent::execute(): Retrieved key[%s] Value[%f]", mKey.value().c_str(), mValue.value());
	    } 
	} else {
	    DBGFMT("CPfloatComponent::execute(): Not found key[%s]", mKey.value().c_str());
	    put(aExec, XINDEX(CPfloatComponent,exists), UFalse());
	}
    }

    //
    // Store self.
    //
    if (mValue.updated() && mKey.value() != "") {
	put(aExec, XINDEX(CPfloatComponent,exists), UTrue()); // It will exist after this one.
	if (db->set(mKey.value(), mValue.value())) 
	    DBGFMT("CPfloatComponent::execute(): Stored  key[%s] value[%f]", mKey.value().c_str(), mValue.value());
	else
	    DBGFMT("CPfloatComponent::execute(): No need to store key[%s] value[%f], value not updated.", mKey.value().c_str(), mValue.value());
	return;
    }	
}


CPstringComponent::CPstringComponent(CExecutor* aExec, CBaseType *aType):
    CExecutable(aExec, aType),
    mValue(this),
    mKey(this)
{
    mValue.putValue(aExec, "");
    mKey.putValue(aExec, "");
    put(aExec, XINDEX(CPstringComponent,exists), UFalse());
    eventPut(aExec,XINDEX(CPstringComponent,value), &mValue);
    eventPut(aExec,XINDEX(CPstringComponent,key), &mKey);
}

void CPstringComponent::start(CExecutor* aExec) 
{
    DBGFMT("CPstringComponent::start(): Called. Key[%s][%d]", 
	   mKey.value().c_str(), mKey.updated());
    mValue.cancel(aExec);
    mKey.setUpdated(aExec);
    execute(aExec);
}

void CPstringComponent::execute(CExecutor* aExec) 
{
    CDatabase *db = CDatabaseComponent::database();
    if (!db)
	return;

    // 
    // If key has been updated, we need to retrieve self from the database, if possible
    //
    if (mKey.updated()) {
	string tmp;

	if (db->get(mKey.value(), &tmp)) {
	    // Don't override newly updated value.
	    if (!mValue.updated()) {
		put(aExec,XINDEX(CPstringComponent,exists), UTrue());
		mValue.putValue(aExec, tmp); // We'll have to do this top activate the assign operator.
		DBGFMT("CPstringComponent::execute(): Retrieved key[%s] Value[%s]", mKey.value().c_str(), mValue.value().c_str());
	    }
	} else {
	    DBGFMT("CPstringComponent::execute(): Not found key[%s]", mKey.value().c_str());
	    put(aExec,XINDEX(CPstringComponent,exists), UFalse());
	}
    }

    //
    // Store self.
    //
    if (mValue.updated() && mKey.value() != "") {
	put(aExec,XINDEX(CPstringComponent,exists), UTrue()); // It will exist after this one.
	if (db->set(mKey.value(), mValue.value())) 
	    DBGFMT("CPstringComponent::execute(): Stored  key[%s] value[%s]", mKey.value().c_str(), mValue.value().c_str());
	else
	    DBGFMT("CPstringComponent::execute(): No need to store key[%s] value[%s], value not updated.", mKey.value().c_str(), mValue.value().c_str());
	return;
    }	
}
