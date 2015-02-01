//
// Event bool scripting is broken.
//
type BoolTrigger {
   event bool t;
};

event bool x;

script x when x == true {
       printf("bugg22: pass\n");
}

//
// If the following three lines are added, the script above
// will execute correctly
//
//script x when x == false {
//       printf("bugg22: no pass\n");
//}

x <- @BoolTrigger { t = true}.t;
