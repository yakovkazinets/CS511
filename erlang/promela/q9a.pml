
byte last = 1;
bool wantP = false;
bool wantQ = false;
byte cs = 0;

active proctype P(){
    do 
        :: wantP = true;
           last = 1;
           !wantQ || last ==2;
		   cs++;
		   assert (cs==1);
		   cs--;
           wantP = false 
    od
}

active proctype Q(){
    do 
        :: wantQ = true;
           last = 2;
           !wantP || last ==1;
		   cs++;
		   assert (cs==1);
		   cs--;
           wantQ = false 
    od
}
