byte c =0;

activate proctype P() {
    c++;
    printf("Value of counter is %d\n",c );
}

activate proctype Q() {
    c++;
    printf("Value of counter is %d\n",c );
}

init {
    atomic {
        run P();
        run Q();
    }
}