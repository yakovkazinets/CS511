byte = 0;
bool reply= false;
byte result;

proctype S(){
    do 
        :: req == 1 ->
            reply = true;
            result = 24;
        :: req == 2 ->
            reply = true;
            result = 54
    od
}

proctype C1(){
    req = 1;
    reply ->
    printf ("Reply is #d\n", result);
}

proctype C2(){
    req = 2;
    reply ->
    printf ("Reply is #d\n", result);
}

init {
    atomic {
        run S();
        run C1();
    }
}
