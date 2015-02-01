//
// All rights reserved. Reproduction, modification, use or disclosure
// to third parties without express authority is forbidden.
// Copyright Magden LLC, California, USA, 2007.
//
#include "m1.hh"
#include "m1vm.hh"

enum InfoStyle { 
    M1_STYLE,    // for system header file
    CPP_STYLE    // for doxygen
};
    
// Dummy needed by all main apps
void VmMain::execute(CExecutor* aExec) { }

char* spaces(int n)
{
    static char sp[161] = "          ""          ""          ""          "
	"          ""          ""          ""          "
	"          ""          ""          ""          "
	"          ""          ""          ""          ";
    return sp+160-n;
}

char* indent(int i)
{
    return spaces(i*2);
}

/* Break text into serveral lines seprated with newline */

int break_text(char* text, int max_line_size, 
	       char** line, int* line_sz, int n)
{
    int i = 0;

next_line:
    while(*text && (i < n)) {
	int   sz;
	int   prev_sz;
	char* prev_text;

	while(*text && isblank(*text))  // skip blanks
	    text++;
	if (*text == '\0')
	    return i;

	sz = prev_sz = 0;
	line[i] = text;
	prev_text = text;

	// scan over a line
	while(*text && (sz < max_line_size)) {
	    // traverse next word
	    while(*text && (sz < max_line_size) &&
		  !isblank(*text) && (*text != '\n')) {
		sz++;
		text++;
	    }

	    if ((*text == '\0') || (*text == '\n')) {
		line_sz[i++] = sz;
		if (*text == '\n') text++;
		goto next_line;
	    }
	    if (sz == max_line_size) {
		if (prev_sz) {
		    line_sz[i++] = prev_sz;
		    text = prev_text;
		}
		else {
		    line_sz[i++] = sz;
		}
		goto next_line;
	    }
		
	    prev_text = text;   // this is the latest word end poistion
	    prev_sz = sz;     // save size including this word 
	    // scan past blanks
	    while(*text && isblank(*text) && (sz < max_line_size)) {
		text++;
		sz++;
	    }
	    if ((sz == max_line_size) || (*text=='\0')) {
		line_sz[i++] = prev_sz;
		goto next_line;
	    }
	}

	if (sz == max_line_size) {
	    if (prev_sz) {
		line_sz[i++] = prev_sz;
		text = prev_text;
	    }
	    else
		line_sz[i++] = sz;
	}
	else
	    line_sz[i++] = sz;
    }
    return i;
}

void export_documentation(FILE* f, int ind, const char* doc)
{
    char* line[1024];
    int   line_sz[1024];
    int   n;
    int   i;
    int max_line_size = 80 - (ind+2)*2;

    if (doc == NULL)
	return;
    n = break_text((char*)doc, max_line_size, line, line_sz, 1024);
    for (i = 0; i < n; i++) {
	fprintf(f, "%s//!  ", indent(ind));
	fwrite(line[i], sizeof(char), line_sz[i], f);
	fprintf(f, "\n");
    }
}

void export_field_cpp(FILE* f, CField* fld, int ind) 
{
    CType* ftype = fld->type();
    string tname;

    if (fld->storage() & Q_PUBLIC) {
	tname = ftype->typeName();
	if (fld->documentation() != NULL)
	    export_documentation(f, ind, fld->documentation());
	fprintf(f, "%s%s %s;\n", indent(ind), tname.c_str(), fld->cname());
    }
}

void export_type_cpp(FILE* f, CType* aType, int ind)
{
    switch(aType->typeTag()) {
    case M1TYPE_BYTE:
    case M1TYPE_CHAR:
    case M1TYPE_BOOL:
    case M1TYPE_SIGNED:
    case M1TYPE_UNSIGNED:
    case M1TYPE_FLOAT:
    case M1TYPE_STRING:
	// fprintf(f, "// TYPE: PRIMITIVE %s\n", aType->cname());
	break;
    case M1TYPE_EVENT:
	// fprintf(f, "// TYPE: EVENT %s\n", aType->cname());	
	break;
    case M1TYPE_ARRAY:
	// fprintf(f, "// TYPE: ARRAY %s\n", aType->cname());
	break;
    case M1TYPE_OBJECT: {
	CBaseType* objType = (CBaseType*) aType;
	int i;

	fprintf(f, "%s//\n", indent(ind));
	fprintf(f, "%s// Type: %s\n", indent(ind), aType->cname());
	if (aType->documentation()) {
	    fprintf(f, "%s// Description\n", indent(ind));
	    export_documentation(f, ind, aType->documentation());
	}
	fprintf(f, "%s//\n", indent(ind));

	if (objType->parentType())
	    fprintf(f, "%sclass %s : public %s {\n", 
		    indent(ind), aType->cname(), 
		    objType->parentType()->cname());
	else
	    fprintf(f, "%sclass %s {\n", 
		    indent(ind), aType->cname());
	fprintf(f, "%spublic:\n", indent(ind));
	for (i = objType->fieldTypeOffset(); i<(int)objType->fieldCount(); i++)
	    export_field_cpp(f, objType->field(i), ind+1);
	fprintf(f, "%s};\n\n", indent(ind));
	break;
    }

    default:
	break;
    }
}

void export_field_m1(FILE* f, CField* fld, int ind)
{
    CType* ftype = fld->type();
    string tname;

    if (fld->storage() & Q_PUBLIC) {
	tname = ftype->typeName();
	if (fld->documentation() != NULL)
	    export_documentation(f, ind, fld->documentation());
	fprintf(f, "%s%s %s;\n", 
		indent(ind), tname.c_str(), fld->cname());
    }
}

void export_type_m1(FILE* f, CType* aType, int ind)
{
    switch(aType->typeTag()) {
    case M1TYPE_BYTE:
    case M1TYPE_CHAR:
    case M1TYPE_BOOL:
    case M1TYPE_SIGNED:
    case M1TYPE_UNSIGNED:
    case M1TYPE_FLOAT:
    case M1TYPE_STRING:
	// fprintf(f, "// TYPE: PRIMITIVE %s\n", aType->cname());
	break;
    case M1TYPE_EVENT:
	// fprintf(f, "// TYPE: EVENT %s\n", aType->cname());	
	break;
    case M1TYPE_ARRAY:
	// fprintf(f, "// TYPE: ARRAY %s\n", aType->cname());
	break;
    case M1TYPE_OBJECT: {
	CBaseType* objType = (CBaseType*) aType;
	int i;

	fprintf(f, "%s//\n", indent(ind));
	fprintf(f, "%s// Type: %s\n", indent(ind), aType->cname());
	if (aType->documentation()) {
	    fprintf(f, "%s// Description\n", indent(ind));
	    export_documentation(f, ind, aType->documentation());
	}
	fprintf(f, "%s//\n", indent(ind));

	if (objType->parentType())
	    fprintf(f, "%sinterface type %s : %s {\n", 
		    indent(ind), aType->cname(), 
		    objType->parentType()->cname());
	else
	    fprintf(f, "%sinterface type %s {\n", 
		    indent(ind), aType->cname());
	for (i = objType->fieldTypeOffset(); i<(int)objType->fieldCount(); i++)
	    export_field_m1(f, objType->field(i), ind+1);
	fprintf(f, "%s};\n", indent(ind));
	break;
    }

    default:
	break;
    }
}

// check if aType is derived from bType
static bool isDerived(CType* aType, CType* bType)
{
    if (aType == bType) // special case for isDerived
	return true;
    while(aType != NULL) {
	if (aType == bType) 
	    return true;
	aType = aType->parentType();
    }
    return false;
}

// FIXME!!!
bool cmp_type(CType* a, CType* b)
{
    bool result;

    if (isDerived(a ,b))
	result = true;
    else if (isDerived(b, a))
	result = false;
    else
	result = a->name() < b->name();
    return result;
}

// push all element having aParent as parent,
//  pushed element are removed from input list
void push_types(list<CType*>* aList, CType* aParent, queue<CType*>* aQueue)
{
    list<CType*>::iterator iter = aList->begin();

    while (iter != aList->end()) {
	if ((*iter)->parentType() == aParent) {
	    aQueue->push(*iter);
	    iter = aList->erase(iter);
	}
	else
	    iter++;
    }
}

list<CType*> depth_sort(list<CType*> aTypeList)
{
    list<CType*>  workList;
    list<CType*>  sortedList;
    queue<CType*> typeQueue;


    workList = aTypeList;  // copy?
    push_types(&workList, NULL, &typeQueue);

    while(!typeQueue.empty()) {
	CType* t = typeQueue.front();
	typeQueue.pop();
	sortedList.push_back(t);
	push_types(&workList, t, &typeQueue);
    }
    return sortedList;
}


int main(int argc, char *argv[])
{
    InfoStyle style = M1_STYLE;
    FILE* out = stdout;
    CBaseType* ctx;
    CBaseType* types;
    list<CType*> obj_types;
    list<CType*>::iterator iter;
    size_t i;

    m1_init(NULL);

    ctx = (CBaseType*) m1_context().type();
    types = ctx->subTypes();

    // Store all object types in obj_types vector
    for (i=0; i < types->fieldCount(); i++) {
	CType* t = types->typeAt(i);

	// push all object types, ignore the dynamic library type
	if ((t->typeTag() == M1TYPE_OBJECT) && (t->name() != ""))
	    obj_types.push_back(t);
    }

    obj_types = depth_sort(obj_types);

    if (argc > 1) {
	if (strcmp(argv[1], "c++") == 0)
	    style = CPP_STYLE;
	else if (strcmp(argv[1], "m1") == 0)
	    style = M1_STYLE;
    }

    if (style == M1_STYLE) {
	int major, minor, patch;

	sscanf(VERSION, "%d.%d.%d", &major,&minor,&patch);

	fprintf(out, "interface library M1 (%d,%d) {\n", major,minor);
	for (iter = obj_types.begin(); iter != obj_types.end(); iter++)
	    export_type_m1(out, *iter, 1);
	fprintf(out, "}\n");
    }
    else if (style == CPP_STYLE) {
	for (iter = obj_types.begin(); iter != obj_types.end(); iter++)
	    export_type_cpp(out, *iter, 1);
    }
    exit(0);
}
