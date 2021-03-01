	switch (t->back) {
	default: Uerror("bad return move");
	case  0: goto R999; /* nothing to undo */

		 /* PROC Q */

	case 3: /* STATE 1 */
		;
		now.nq = trpt->bup.oval;
		;
		goto R999;

	case 4: /* STATE 2 */
		;
		now.nq = trpt->bup.oval;
		;
		goto R999;

	case 5: /* STATE 3 */
		;
		now.nq = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 7: /* STATE 5 */
		;
		now.nq = trpt->bup.oval;
		;
		goto R999;

	case 8: /* STATE 9 */
		;
		p_restor(II);
		;
		;
		goto R999;

		 /* PROC P */

	case 9: /* STATE 1 */
		;
		now.np = trpt->bup.oval;
		;
		goto R999;

	case 10: /* STATE 2 */
		;
		now.np = trpt->bup.oval;
		;
		goto R999;

	case 11: /* STATE 3 */
		;
		now.np = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 13: /* STATE 5 */
		;
		now.np = trpt->bup.oval;
		;
		goto R999;

	case 14: /* STATE 9 */
		;
		p_restor(II);
		;
		;
		goto R999;
	}

