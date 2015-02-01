
const bool use_kph = true;
const bool use_psi = true;
const bool use_celsius = true;

const float pwidth = 120;
const float col1 = 4;
const float col2 = 128;
const float col3 = 256;
const float col4 = 384;
const float col5 = 512;

const float pheight = 80;
const float row1 = 5;
const float row2 = 90;
const float row3 = 180;
const float row4 = 270;


Sampler spd_sampler;		// Speed (kph/mph)
Sampler rpm_sampler;		// Revolutions per minute
Sampler lam_1_1_sampler;	// Lambda 1
Sampler lam_1_2_sampler;	// Lambda 2
Sampler lam_2_1_sampler;	// 
Sampler lam_2_2_sampler;
Sampler load_sampler;		// Engine load 0-100%
Sampler ect_sampler;		// Engine coolant temperature,(f/c)
Sampler iat_sampler;	    	// Intake air temperature (f/c)
Sampler aat_sampler;	    	// Ambient air temperature (f/c)
Sampler eot_sampler;		// Engine oil temperature, (f/c)
Sampler vbat_sampler;		// Battery voltage
Sampler ign_adv_sampler;	// Timing advance degrees relative #1 cyl
Sampler maf_sampler;		// Mass Air Flow Rate grams/sec
Sampler tps_sampler;		// Throttle position 0-100%
Sampler map_sampler;		// Manifold air pressure (mbar/psi)
Sampler fp_sampler;		// Fuel pressure (mbar/psi)
Sampler bap_sampler;		// Barometric (ambient) air pressure (mbar/psi)
Sampler fuel_lev_sampler;	// Fuel level 0-100%

Plot spd_plot;
Plot rpm_plot;
Plot map_plot;
Plot lam_1_1_plot;
Plot lam_1_2_plot;
Plot lam_2_1_plot;
Plot lam_2_2_plot;
Plot load_plot;
Plot ect_plot;
Plot iat_plot;
Plot aat_plot;
Plot eot_plot;
Plot vbat_plot;
Plot ign_adv_plot;
Plot maf_plot;
Plot tps_plot;
Plot fp_plot;
Plot bap_plot;
Plot fuel_lev_plot;

Screen screen;

rpm_sampler     = @Sampler { value <- .Engine.engine.rpm_norm.value };
load_sampler    = @Sampler { value <- .Engine.engine.load_norm.value };
vbat_sampler    = @Sampler { value <- .Engine.engine.vbat_norm.value };
ign_adv_sampler = @Sampler { value <- .Engine.engine.ign_adv_norm.value };
maf_sampler     = @Sampler { value <- .Engine.engine.maf_norm.value };
tps_sampler     = @Sampler { value <- .Engine.engine.tps_norm.value };
fuel_lev_sampler = @Sampler { value <- .Engine.engine.fuel_lev_norm.value };
lam_1_1_sampler = @Sampler { value <- .Engine.engine.lam_1_1_norm.value };
lam_1_2_sampler = @Sampler { value <- .Engine.engine.lam_1_2_norm.value };
lam_2_1_sampler = @Sampler { value <- .Engine.engine.lam_2_1_norm.value };
lam_2_2_sampler = @Sampler { value <- .Engine.engine.lam_2_2_norm.value };

if (use_kph)
   spd_sampler = @Sampler { value <- .Engine.engine.kph_norm.value };
else
   spd_sampler = @Sampler { value <- .Engine.engine.mph_norm.value };

if (use_psi) {
   map_sampler = @Sampler { value <- .Engine.engine.map_psi_norm.value };
   fp_sampler  = @Sampler { value <- .Engine.engine.fp_psi_norm.value };
   bap_sampler = @Sampler { value <- .Engine.engine.bap_psi_norm.value };
}
else {
   map_sampler = @Sampler { value <- .Engine.engine.map_mbar_norm.value };
   fp_sampler  = @Sampler { value <- .Engine.engine.fp_mbar_norm.value };
   bap_sampler = @Sampler { value <- .Engine.engine.bap_mbar_norm.value };
}


if (use_celsius) {
   ect_sampler = @Sampler { value <- .Engine.engine.ect_c_norm.value };
   iat_sampler = @Sampler { value <- .Engine.engine.iat_c_norm.value };
   aat_sampler = @Sampler { value <- .Engine.engine.aat_c_norm.value };
   eot_sampler = @Sampler { value <- .Engine.engine.eot_c_norm.value };
}
else {
   ect_sampler = @Sampler { value <- .Engine.engine.ect_f_norm.value };
   iat_sampler = @Sampler { value <- .Engine.engine.iat_f_norm.value };
   aat_sampler = @Sampler { value <- .Engine.engine.aat_f_norm.value };
   eot_sampler = @Sampler { value <- .Engine.engine.eot_f_norm.value };
}


// column 1
rpm_plot = 
     @Plot { top = row1, left = col1,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0xff0000,
	     samples = rpm_sampler
	     };

spd_plot = 
     @Plot { top = row2, left = col1,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x00ff00,
	     samples = spd_sampler
	     };

load_plot = 
     @Plot { top = row3, left = col1,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = load_sampler
	     };

vbat_plot = 
     @Plot { top = row4, left = col1,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = vbat_sampler
	     };

// column = 2
lam_1_1_plot = 
     @Plot { top = row1, left = col2,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = lam_1_1_sampler };
     	   
lam_1_2_plot =
     @Plot { top = row2, left = col2,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = lam_1_2_sampler };

lam_2_1_plot = 
     @Plot { top = row3, left = col2,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = lam_2_1_sampler };

lam_2_2_plot = 
     @Plot { top = row4, left = col2,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = lam_2_2_sampler };

// column = 3
ect_plot = 
     @Plot { top = row1, left = col3,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = ect_sampler };
     	   
iat_plot =
     @Plot { top = row2, left = col3,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = iat_sampler };

aat_plot = 
     @Plot { top = row3, left = col3,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = aat_sampler };

eot_plot = 
     @Plot { top = row4, left = col3,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = eot_sampler };


// column = 4
map_plot = 
     @Plot { top = row1, left = col4,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = map_sampler };
     	   
fp_plot =
     @Plot { top = row2, left = col4,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = fp_sampler };

bap_plot = 
     @Plot { top = row3, left = col4,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = bap_sampler };

fuel_lev_plot  = 
     @Plot { top =  row4, left = col4,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = fuel_lev_sampler };

// column = 5
ign_adv_plot =
     @Plot { top = row1, left = col5,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = ign_adv_sampler };

maf_plot = 
     @Plot { top = row2, left = col5,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = maf_sampler };

tps_plot  = 
     @Plot { top = row3, left = col5,
	     contentWidth = pwidth, contentHeight = pheight,
	     background = true,
	     backgroundColor = 0xcccccc,
	     plotType = spike,
     	     plotTime = 5,
	     color = 0x0000ff,
	     samples = tps_sampler };

screen = 
   @Screen { height = 480, width  = 640,
   	     pixelType = "bgra",
	     backendType = "x11",
	     background = true,
	     backgroundColor = 0x00000000,
	     children = {
	     	      spd_plot, rpm_plot, map_plot,
	     	      lam_1_1_plot, lam_1_2_plot, lam_2_1_plot,lam_2_2_plot,
		      load_plot, ect_plot, iat_plot, aat_plot, eot_plot,
		      vbat_plot, ign_adv_plot, maf_plot, tps_plot,
		      fp_plot, bap_plot, fuel_lev_plot }
};
