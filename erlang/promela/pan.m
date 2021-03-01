#define rand	pan_rand
#if defined(HAS_CODE) && defined(VERBOSE)
	cpu_printf("Pr: %d Tr: %d\n", II, t->forw);
#endif
	switch (t->forw) {
	default: Uerror("bad forward move");
	case 0:	/* if without executable clauses */
		continue;
	case 1: /* generic 'goto' or 'skip' */
		IfNotBlocked
		_m = 3; goto P999;
	case 2: /* generic 'else' */
		IfNotBlocked
		if (trpt->o_pm&1) continue;
		_m = 3; goto P999;

		 /* PROC Q */
	case 3: /* STATE 1 - bakery.pml:21 - [nq = 1] (0:0:1 - 1) */
		IfNotBlocked
		reached[1][1] = 1;
		(trpt+1)->bup.oval = ((int)now.nq);
		now.nq = 1;
#ifdef VAR_RANGES
		logval("nq", ((int)now.nq));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 4: /* STATE 2 - bakery.pml:22 - [nq = np] (0:0:1 - 1) */
		IfNotBlocked
		reached[1][2] = 1;
		(trpt+1)->bup.oval = ((int)now.nq);
		now.nq = ((int)now.np);
#ifdef VAR_RANGES
		logval("nq", ((int)now.nq));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 5: /* STATE 3 - bakery.pml:23 - [nq = (nq+1)] (0:0:1 - 1) */
		IfNotBlocked
		reached[1][3] = 1;
		(trpt+1)->bup.oval = ((int)now.nq);
		now.nq = (((int)now.nq)+1);
#ifdef VAR_RANGES
		logval("nq", ((int)now.nq));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 6: /* STATE 4 - bakery.pml:24 - [(((np==0)||(nq<np)))] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][4] = 1;
		if (!(((((int)now.np)==0)||(((int)now.nq)<((int)now.np)))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 7: /* STATE 5 - bakery.pml:26 - [nq = 0] (0:0:1 - 1) */
		IfNotBlocked
		reached[1][5] = 1;
		(trpt+1)->bup.oval = ((int)now.nq);
		now.nq = 0;
#ifdef VAR_RANGES
		logval("nq", ((int)now.nq));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 8: /* STATE 9 - bakery.pml:29 - [-end-] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][9] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */

		 /* PROC P */
	case 9: /* STATE 1 - bakery.pml:10 - [np = 1] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][1] = 1;
		(trpt+1)->bup.oval = ((int)now.np);
		now.np = 1;
#ifdef VAR_RANGES
		logval("np", ((int)now.np));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 10: /* STATE 2 - bakery.pml:11 - [np = nq] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][2] = 1;
		(trpt+1)->bup.oval = ((int)now.np);
		now.np = ((int)now.nq);
#ifdef VAR_RANGES
		logval("np", ((int)now.np));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 11: /* STATE 3 - bakery.pml:12 - [np = (np+1)] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][3] = 1;
		(trpt+1)->bup.oval = ((int)now.np);
		now.np = (((int)now.np)+1);
#ifdef VAR_RANGES
		logval("np", ((int)now.np));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 12: /* STATE 4 - bakery.pml:13 - [(((nq==0)||(np<=nq)))] (0:0:0 - 1) */
		IfNotBlocked
		reached[0][4] = 1;
		if (!(((((int)now.nq)==0)||(((int)now.np)<=((int)now.nq)))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 13: /* STATE 5 - bakery.pml:15 - [np = 0] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][5] = 1;
		(trpt+1)->bup.oval = ((int)now.np);
		now.np = 0;
#ifdef VAR_RANGES
		logval("np", ((int)now.np));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 14: /* STATE 9 - bakery.pml:17 - [-end-] (0:0:0 - 1) */
		IfNotBlocked
		reached[0][9] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */
	case  _T5:	/* np_ */
		if (!((!(trpt->o_pm&4) && !(trpt->tau&128))))
			continue;
		/* else fall through */
	case  _T2:	/* true */
		_m = 3; goto P999;
#undef rand
	}

