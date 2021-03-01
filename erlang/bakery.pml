/*Yakov Kazinets */
/*I pledge my honor that I have abided by the Stevens Honor System. */
/*December 9, 2020 */

byte np = 0;
byte nq = 0;

active proctype P(){
    do 
        :: np = 1;
		   np = nq;
           np++;
           nq ==0|| np <= nq;
		   
           np=0;
    od
}

active proctype Q(){
    do 
        :: nq = 1;
		   nq = np;
           nq++;
           np ==0 || nq < np;
		   
           nq=0;
		   
    od
}