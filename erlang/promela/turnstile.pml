byte counter = 0;
//byte N = 50; /* number of people that go in, per turnstile */ 

proctype T1() {
    byte i = 1;
    do
    :: i>50 -> break
    :: else ->
       counter++;
       i++;
    od
}


proctype T2() {
    byte i = 1;
    do
    :: i>50 -> break
    :: else ->
       counter++;
       i++;
    od
}


init {
    atomic {
        run T1();
        run T2()
    }
    _nr_pr==1;
    printf("Counter %d\n",counter)
}
