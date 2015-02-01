
#include "m1.hh"
#include "color.hh"

OBJECT_TYPE_BOOTSTRAP(CColor);

BASIC_EVENT_TYPE(CColor, u, unsigned int);
QUEUE_EVENT_TYPE(CColor, u, unsigned int);

UData CColor::CColorType::produceEvent(CExecutor* aExec, bool aQueued) 
{
    UData r;
    if (aQueued)
	r.evt = new EventQueueCColor(aExec->current());
    else
	r.evt = new EventCColor(aExec->current());
    return r;
}

/*** 
void CColorType::print(ostream* os, UData color)
{
    EPixel_t pixel;
    pixel.px = color;
    *os << "Color { " << 
	"a=" << pixel.a << 
	",r=" << pixel.r <<
	",g=" << pixel.g <<
	",b=" << pixel.b;
}
**/
