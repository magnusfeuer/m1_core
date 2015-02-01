
#include <math.h> // For sinf
#include "m1.hh"
#include "m1vm.hh"
#include "dds_component.hh"
#include "time_sensor.hh"
#include "screen_component.hh"
#include "geometric.hh"
#include "epic_keyboard_device.hh"
#include "epic_touch_device.hh"
#include "scalar_interpolator.hh"
#include "color_interpolator.hh"
#include "position_interpolator.hh"

int main(int argc, char *argv[])
{
    int child_index = 0;

    m1_init(0); // Add .so library directories here if needed.
    epic_init(EPIC_SIMD_AUTO);

    CTimer* timer = new CTimer();
    CScalarInterpolatorComponent *cycle = new CScalarInterpolatorComponent();    
    CDDSComponent *background = new CDDSComponent();
    CDDSComponent *ul_accel = new CDDSComponent();
    CLayerComponent *upper_left = new CLayerComponent();
    CEpicScreenComponent *epic = new CEpicScreenComponent();


    // Setup daddy sweeper
    CSweeper::singleton()->addComponent(epic);
    CSweeper::singleton()->addComponent(timer);
    CSweeper::singleton()->addComponent(cycle);


    // Temp test.

    //
    // Configure the timer component
    //

    // delay start 3s
    timer->put("startTime", UUnsigned(CSweeper::singleton()->timeStamp() + 3000ULL));
    // restart every 3s
    timer->put("cycleInterval", UUnsigned(3000ULL));
    timer->put("loop", UTrue());
    timer->put("enabled", UTrue());

    //
    // Setup cycle scalar interpolator
    //
    CArray *cycle_keyValue = new CArray(CArrayType::create(float_type(), 0), sizeof(float), 19);
    CArray *cycle_key      = new CArray(CArrayType::create(float_type(), 0), sizeof(float), 19);

    //
    // Setup cycle key to sin from 0-180 degrees.
    //
    for(int i = 0; i <= 18; ++i) {
	float key = i/18.0;
	float value = sinf(key*2*M_PI);
	cycle_key->put(i, UFloat(key));
	cycle_keyValue->put(i, UFloat(value));
    }

    cycle->put("key", UObject(cycle_key));
    cycle->put("keyValue", UObject(cycle_keyValue));

    //
    // Tie the sequencer output to cycle key input
    //
    cycle->connect("fraction", timer, "fraction");

    //
    // Background image 
    //
    background->put("top", USigned(0));
    background->put("left", USigned(0));
    background->put("dds_file", UString(new CString("TB-BACKGROUND-BlueGlow.dds")));    
    background->put("alpha_fader", UFloat(1.0));
    background->put("visible", UTrue());
    background->put("enabled", UTrue());

    //
    // Upper left instrument layer with DDS.
    //
    upper_left->put("top", USigned(23));
    upper_left->put("left", USigned(43));
    upper_left->put("visible", UTrue());
    
    // Accelerometer
    ul_accel->put("top", USigned(0));
    ul_accel->put("left", USigned(0));
    ul_accel->put("dds_file", UString(new CString("BlueGlow-TB-Small-Solid-Normal.dds")));
    ul_accel->put("alpha_fader", UFloat(1.0));
    ul_accel->put("visible", UTrue());
    ul_accel->put("enabled", UTrue());
    ul_accel->connect("value", cycle, "value");
    ul_accel->connect("alpha_fader", cycle, "value");

    cycle->put("value", UFloat(1.0));

    // Install ul_accel as a child under upper_left layer
    upper_left->at("children").arr->put(child_index++, UObject(ul_accel));

    //
    // Setup Epic component
    //
    epic->put("height", USigned(480));
    epic->put("width",  USigned(640));

    // FIXME strings!!!
    epic->put("pixel_type", UString(new CString("bgra"))); // Will this UString leak? I think not.
    epic->put("backend_type", UString(new CString("auto")));
    epic->put("framebuffer_device", UString(new CString("/dev/fb0")));
    
    cout << "EPIC: ";
    epic->type()->print(&cout, UUnsigned((unsigned int)epic));
    cout << "\n";

    //
    // Add background  and layer as children to screen
    //
    epic->at(0).arr->put(child_index++, UObject(background)); 
    epic->at(0).arr->put(child_index++, UObject(upper_left)); 

    //
    // Check that all components are correctly configured.
    //
    cout << "TIMER: ";
    timer->type()->print(&cout, UUnsigned((unsigned int)timer));
    cout << "\n";
    
    epic->enterGraphicsMode();

    while(true) {
	CSweeper::singleton()->sweep();
	epic->redraw();
    }
}
