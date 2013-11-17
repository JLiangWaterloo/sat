/***************************************************************************************[Solver.cc]
Copyright (c) 2003-2006, Niklas Een, Niklas Sorensson
Copyright (c) 2007-2010, Niklas Sorensson

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**************************************************************************************************/

#include <math.h>

#include "mtl/Sort.h"
#include "Solver.h"

using namespace Minisat;

//=================================================================================================
// Options:


static const char* _cat = "CORE";

static DoubleOption  opt_var_decay         (_cat, "var-decay",   "The variable activity decay factor",            0.95,     DoubleRange(0, false, 1, false));
static DoubleOption  opt_clause_decay      (_cat, "cla-decay",   "The clause activity decay factor",              0.999,    DoubleRange(0, false, 1, false));
static DoubleOption  opt_random_var_freq   (_cat, "rnd-freq",    "The frequency with which the decision heuristic tries to choose a random variable", 0.02, DoubleRange(0, true, 1, true));
static DoubleOption  opt_random_seed       (_cat, "rnd-seed",    "Used by the random variable selection",         91648253, DoubleRange(0, false, HUGE_VAL, false));
static IntOption     opt_ccmin_mode        (_cat, "ccmin-mode",  "Controls conflict clause minimization (0=none, 1=basic, 2=deep)", 2, IntRange(0, 2));
static IntOption     opt_phase_saving      (_cat, "phase-saving", "Controls the level of phase saving (0=none, 1=limited, 2=full)", 0, IntRange(0, 2));
static BoolOption    opt_rnd_init_act      (_cat, "rnd-init",    "Randomize the initial activity", false);
static BoolOption    opt_luby_restart      (_cat, "luby",        "Use the Luby restart sequence", false);
static IntOption     opt_restart_first     (_cat, "rfirst",      "The base restart interval", 100, IntRange(1, INT32_MAX));
static DoubleOption  opt_restart_inc       (_cat, "rinc",        "Restart interval increase factor", 1.5, DoubleRange(1, false, HUGE_VAL, false));
static DoubleOption  opt_garbage_frac      (_cat, "gc-frac",     "The fraction of wasted memory allowed before a garbage collection is triggered",  0.20, DoubleRange(0, false, HUGE_VAL, false));
static IntOption     opt_slowdown_pure     (_cat, "slowdown_pure", "Controls which restart level to slown down the pure literection ", 0, IntRange(0, INT32_MAX));
static IntOption     opt_stop_pure         (_cat, "stop_pure",     "Controls which restart level to stop the pure literal detection ", 1, IntRange(0, INT32_MAX));
static IntOption     opt_freq_pure         (_cat, "freq_pure",     "After stop pure literal detection after how much restart level start again pure literal detetion ", 5, IntRange(0, INT32_MAX));
static BoolOption    opt_dis_act           (_cat, "dis-act",     "Disable the activity heuristic", false);
static BoolOption    opt_dis_learn         (_cat, "dis-learn",   "Disable the learning heuristic", false);
static BoolOption    opt_dis_bj            (_cat, "dis-bj",      "Disable backjumping", false);
static BoolOption    opt_dis_restart       (_cat, "dis-restart", "Disable restarts", false);
static StringOption  opt_dump_decision     (_cat, "dump-decision", "If given, dump the decisions");
static StringOption  opt_dump_file         (_cat, "dump-file", "If given, dump the dimacs file of the internal solver during solving");
static IntOption     opt_dump_freq         (_cat, "dump-freq", "How often to dump.", 1, IntRange(1, INT32_MAX));

//=================================================================================================
// Constructor/Destructor:


Solver::Solver() :

    // Parameters (user settable):
    //
    verbosity        (0)
  , var_decay        (opt_var_decay)
  , clause_decay     (opt_clause_decay)
  , random_var_freq  (opt_random_var_freq)
  , random_seed      (opt_random_seed)
  , luby_restart     (opt_luby_restart)
  , ccmin_mode       (opt_ccmin_mode)
  , phase_saving     (opt_phase_saving)
  , rnd_pol          (false)
  , rnd_init_act     (opt_rnd_init_act)
  , garbage_frac     (opt_garbage_frac)
  , restart_first    (opt_restart_first)
  , restart_inc      (opt_restart_inc)
  , slowdown_pure    (opt_slowdown_pure)
  , stop_pure        (opt_stop_pure)
  , freq_pure        (opt_freq_pure)
  , dis_act          (opt_dis_act)
  , dis_learn        (opt_dis_learn)
  , dis_bj           (opt_dis_bj)
  , dis_restart      (opt_dis_restart)
  , dump_decision    (opt_dump_decision)
  , dump_file        ((const char*) opt_dump_file)
  , dump_file_stream (NULL)
  , dump_freq        (opt_dump_freq)
  , dump_counter     (0)
    // Parameters (the rest):
    //
  , learntsize_factor((double)1/(double)3), learntsize_inc(1.1)

    // Parameters (experimental):
    //
  , learntsize_adjust_start_confl (105)
  , learntsize_adjust_inc         (1.5)

    // Statistics: (formerly in 'SolverStats')
    //
  , solves(0), starts(0), decisions(0), rnd_decisions(0), propagations(0), conflicts(0)
  , dec_vars(0), clauses_literals(0), learnts_literals(0), max_literals(0), tot_literals(0)
  , learnt_clauses(0), learnt_asserting(0)

  , ok                 (true)
  , cla_inc            (1)
  , var_inc            (1)
  , watches            (WatcherDeleted(ca))
  , qhead              (0)
  , simpDB_assigns     (-1)
  , simpDB_props       (0)
  , order_heap         (VarOrderLt(activity))
  , progress_estimate  (0)
  , remove_satisfied   (true)

    // Resource constraints:
    //
  , conflict_budget    (-1)
  , propagation_budget (-1)
  , asynch_interrupt   (false)
  , clause_trail_sz    (0)
  , tenth_var(0)
  , fourth_var(0)
  , half_var(0)
  , threefourth_var(0)
  , restart(0)
  , implement_pure(false)
  , begin_solve(false)
  
{
   
}


Solver::~Solver()
{
}


//=================================================================================================
// Minor methods:


// Creates a new SAT variable in the solver. If 'decision' is cleared, variable will not be
// used as a decision variable (NOTE! This has effects on the meaning of a SATISFIABLE result).
//
Var Solver::newVar(bool sign, bool dvar)
{
    int v = nVars();
	vec<int> * empty1=new vec<int>; empty1->ini_cap(16); 
	vec<int> * empty2=new vec<int>; empty2->ini_cap(16);
	lit_cla1.push(empty1);
	lit_cla0.push(empty2);
    watches  .init(mkLit(v, false));
    watches  .init(mkLit(v, true ));
    assigns  .push(l_Undef);
    vardata  .push(mkVarData(CRef_Undef, 0));
    //activity .push(0);
    activity .push(rnd_init_act ? drand(random_seed) * 0.00001 : 0);
    seen     .push(0);
    polarity .push(sign);
    decision .push();
    trail    .capacity(v+1);
    setDecisionVar(v, dvar);

    return v;
}


bool Solver::addClause_(vec<Lit>& ps)
{
    assert(decisionLevel() == 0);
    if (!ok) return false;

    // Check if clause is satisfied and remove false/duplicate literals:
    sort(ps);
	
   vec<Lit>    oc;
    oc.clear();

    Lit p; int i, j, flag = 0;
    for (i = j = 0, p = lit_Undef; i < ps.size(); i++) {
        oc.push(ps[i]);
        if (value(ps[i]) == l_True || ps[i] == ~p || value(ps[i]) == l_False)
          flag = 1;
    }
   
   
   
    for (i = j = 0, p = lit_Undef; i < ps.size(); i++)
        if (value(ps[i]) == l_True || ps[i] == ~p)
            return true;
        else if (value(ps[i]) != l_False && ps[i] != p)
            ps[j++] = p = ps[i];
    ps.shrink(i - j);


if (flag && (output != NULL)) {
      for (i = j = 0, p = lit_Undef; i < ps.size(); i++)
        fprintf(output, "%i ", (var(ps[i]) + 1) * (-2 * sign(ps[i]) + 1));
      fprintf(output, "0\n");

      fprintf(output, "d ");
      for (i = j = 0, p = lit_Undef; i < oc.size(); i++)
        fprintf(output, "%i ", (var(oc[i]) + 1) * (-2 * sign(oc[i]) + 1));
      fprintf(output, "0\n");
    }





    if (ps.size() == 0)
        return ok = false;
    else if (ps.size() == 1){
        uncheckedEnqueue(ps[0]);
        return ok = (propagate(true) == CRef_Undef);
    }
	else{
         CRef cr = ca.alloc(ps, false);
		//ClaData cd=ClaData(cr,ps.size());
		
        clauses.push(cr);
        attachClause(cr);
		
		
		//cla_dec.push(false);
    }

    return true;
}


void Solver::attachClause(CRef cr) {
    const Clause& c = ca[cr];
    assert(c.size() > 1);
    watches[~c[0]].push(Watcher(cr, c[1]));
    watches[~c[1]].push(Watcher(cr, c[0]));
    if (c.learnt()) learnts_literals += c.size();
    else            clauses_literals += c.size(); }


void Solver::detachClause(CRef cr, bool strict) {
   // printf("in detachCla\n");
    const Clause& c = ca[cr];
    assert(c.size() > 1);
    
    if (strict){
        remove(watches[~c[0]], Watcher(cr, c[1]));
        remove(watches[~c[1]], Watcher(cr, c[0]));
    }else{
        // Lazy detaching: (NOTE! Must clean all watcher lists before garbage collecting this clause)
        watches.smudge(~c[0]);
        watches.smudge(~c[1]);
    }

    if (c.learnt()) learnts_literals -= c.size();
    else            clauses_literals -= c.size(); 
	//printf("after detachCla\n");
	}


void Solver::removeClause(CRef cr) {
   // printf("in remove cla\n");
    Clause& c = ca[cr];
    
    if (output != NULL) {
      fprintf(output, "d ");
      for (int i = 0; i < c.size(); i++)
        fprintf(output, "%i ", (var(c[i]) + 1) * (-2 * sign(c[i]) + 1));
      fprintf(output, "0\n");
    }
    
    
    
    
    
    detachClause(cr);
    // Don't leave pointers to free'd memory!
    if (locked(c)) vardata[var(c[0])].reason = CRef_Undef;
	
    c.mark(1); 
    ca.free(cr);
	//printf("after remove cla\n");
}


bool Solver::satisfied(const Clause& c) const {
    //printf("in satisfied\n");
    //printf("c-size:%d\n",c.size());
    for (int i = 0; i < c.size(); i++)
       { 	//printf("var:c[i]=%d\n",var(c[i]));
	 //  printf("var[i]=%d\n ",var(c[i]));
	   if (value(c[i]) == l_True)
           { //printf("out of  sat\n");
		   return true;}}
		   //printf("out of sat\n");
    return false; }


// Revert to the state at given level (keeping all assignment at 'level' but not beyond).
//
void Solver::cancelUntil(int level) {
    if (decisionLevel() > level){
	//printf("in canceluntil\n");
        for (int c = trail.size()-1; c >= trail_lim[level]; c--){
            Var      x  = var(trail[c]);
            assigns [x] = l_Undef; 
			//if( vardata[x].reason==CRef_Pure)    
            vardata[x].reason=CRef_Undef  ;
            if (phase_saving > 1 || (phase_saving == 1) && c > trail_lim.last())
                polarity[x] = sign(trail[c]);
            insertVarOrder(x); 
			}
			
		//Var y= var(trail[trail_lim[level]]);
		//polarity[y]= !sign(trail[trail_lim[level]]);
		
        qhead = trail_lim[level];
        trail.shrink(trail.size() - trail_lim[level]);
        trail_lim.shrink(trail_lim.size() - level);
		
		//printf("before cancel until for\n");
		for(int m=clause_trail_sz-1;m>=clause_trail_lim[level];m--)
		{
		    ClaData& cl= clauses_data[clause_trail[m]];
		    cl.is_sat=false;
			
		}
		//printf("after cancel until for\n");
		clause_trail_sz=clause_trail_lim[level];
		
		//printf("after cancle until\n");
    } }


//=================================================================================================
// Major methods:


Lit Solver::pickBranchLit()
{

    Var next = var_Undef;

    // Random decision:
    if (drand(random_seed) < random_var_freq && !order_heap.empty()){
        next = order_heap[irand(random_seed,order_heap.size())];
        if (value(next) == l_Undef && decision[next])
            rnd_decisions++; }

    // Activity based decision:
    while (next == var_Undef || value(next) != l_Undef || !decision[next])
        if (order_heap.empty()){
            next = var_Undef;
            break;
        }else
            next = order_heap.removeMin();
    
	bool bb;
	if(next!=var_Undef)
	{
	bb=rnd_pol ? drand(random_seed) < 0.5 : polarity[next];
	vec<int> * t=(bb)?lit_cla1[next]:lit_cla0[next];
	if(t->size()==0){bb=!bb; polarity[next]=bb;}
	return mkLit(next, bb);
	}
	else {return lit_Undef;}
   // return next == var_Undef ? lit_Undef : mkLit(next, bb);
  // printf("after pick branch\n");
}


/*_________________________________________________________________________________________________
|
|  analyze : (confl : Clause*) (out_learnt : vec<Lit>&) (out_btlevel : int&)  ->  [void]
|  
|  Description:
|    Analyze conflict and produce a reason clause.
|  
|    Pre-conditions:
|      * 'out_learnt' is assumed to be cleared.
|      * Current decision level must be greater than root level.
|  
|    Post-conditions:
|      * 'out_learnt[0]' is the asserting literal at level 'out_btlevel'.
|      * If out_learnt.size() > 1 then 'out_learnt[1]' has the greatest decision level of the 
|        rest of literals. There may be others from the same level though.
|  
|________________________________________________________________________________________________@*/
void Solver::analyze(CRef confl, vec<Lit>& out_learnt, int& out_btlevel)
{
    if (dis_bj) {
        out_btlevel = decisionLevel() - 1;
        out_learnt.push(~trail[trail_lim[out_btlevel]]);
        return;
    }
    
  //  printf("in analyze\n");
    int pathC = 0;
    Lit p     = lit_Undef;

    // Generate conflict clause:
    //
    out_learnt.push();      // (leave room for the asserting literal)
    int index   = trail.size() - 1;

    do{
        //assert(confl != CRef_Undef); // (otherwise should be UIP)
		
		if(confl==CRef_Pure)
		{  
		     vec<int> *t=sign(p)?lit_cla0[var(p)]:lit_cla1[var(p)];
			 //printf("var(p)=%d\n",var(p));
			 if(t->size()>0)
			 {
				 for (int j = 0 ; j < t->size(); j++){
				   // printf("t.size():%d\n",t->size());
					Lit q = clauses_data[(*t)[j]].dec_lit;
					//printf("var(q)=%d\n",var(q));
					//printf("after bum act, size= %d \n", c.size()) ;
				   // printf("var q before :%d, j= %d\n", var(q), j);
					if (!seen[var(q)] && level(var(q)) > 0){
					 //   printf("before var bump\n");
						varBumpActivity(var(q));
					//	printf("after var bump\n");
						seen[var(q)] = 1;
					//	printf("var(q)=%d \n", var(q) );
						if (level(var(q)) >= decisionLevel())
							pathC++;
						else
							out_learnt.push(q);
					}
					//printf("after for loop\n");
				}
			}
		}
		else
		{
			Clause& c = ca[confl];

			if (c.learnt())
		   {      claBumpActivity(c);}
		   
			 
			for (int j = (p == lit_Undef) ? 0 : 1; j < c.size(); j++){
			   // printf("c.size():%d\n",c.size());
				Lit q = c[j];
				//printf("var(q)=%d\n",var(q));
				//printf("after bum act, size= %d \n", c.size()) ;
			   // printf("var q before :%d, j= %d\n", var(q), j);
				if (!seen[var(q)] && level(var(q)) > 0){
				 //   printf("before var bump\n");
					varBumpActivity(var(q));
				//	printf("after var bump\n");
					seen[var(q)] = 1;
				//	printf("var(q)=%d \n", var(q) );
					if (level(var(q)) >= decisionLevel())
						pathC++;
					else
						out_learnt.push(q);
				}
				//printf("after for loop\n");
			}
        }
        // Select next clause to look at:
        while (!seen[var(trail[index--])]);
		//printf("begin trail\n");
        p     = trail[index+1];
		//printf("var(p):%d\n", var(p));
        confl = reason(var(p)); 
        seen[var(p)] = 0;
        pathC--;
		
		//printf("after trail\n");

    }while (pathC > 0);
	//if(p==lit_Undef)
    // {printf("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");}
    out_learnt[0] = ~p;
   //printf("after do\n");
    // Simplify conflict clause:
    //
    int i, j;
    out_learnt.copyTo(analyze_toclear);
    if (ccmin_mode == 2){
        uint32_t abstract_level = 0;
        for (i = 1; i < out_learnt.size(); i++)
            abstract_level |= abstractLevel(var(out_learnt[i])); // (maintain an abstraction of levels involved in conflict)
       //printf("begin for\n");
        for (i = j = 1; i < out_learnt.size(); i++)
            if (reason(var(out_learnt[i])) == CRef_Undef || !litRedundant(out_learnt[i], abstract_level))
                out_learnt[j++] = out_learnt[i];
       //printf("after for\n");
    }else if (ccmin_mode == 1){
        for (i = j = 1; i < out_learnt.size(); i++){
            Var x = var(out_learnt[i]);

            if (reason(x) == CRef_Undef)
                out_learnt[j++] = out_learnt[i];
            else{
			    //printf("in reason\n");
                Clause& c = ca[reason(var(out_learnt[i]))];
				//printf("after reason\n");
                for (int k = 1; k < c.size(); k++)
                    if (!seen[var(c[k])] && level(var(c[k])) > 0){
                        out_learnt[j++] = out_learnt[i];
                        break; }
            }
        }
    }else
        i = j = out_learnt.size();

    max_literals += out_learnt.size();
    out_learnt.shrink(i - j);
    tot_literals += out_learnt.size();
    
	//printf("after find\n");
    // Find correct backtrack level:
    //
    if (out_learnt.size() == 1)
        out_btlevel = 0;
    else{
        int max_i = 1;
        // Find the first literal assigned at the next-highest level:
        for (int i = 2; i < out_learnt.size(); i++)
            if (level(var(out_learnt[i])) > level(var(out_learnt[max_i])))
                max_i = i;
        // Swap-in this literal at index 1:
        Lit p             = out_learnt[max_i];
        out_learnt[max_i] = out_learnt[1];
        out_learnt[1]     = p;
        out_btlevel       = level(var(p));
    }
    
    for (int j = 0; j < analyze_toclear.size(); j++) seen[var(analyze_toclear[j])] = 0;    // ('seen[]' is now cleared)
	//printf("out of analyze\n");
}


// Check if 'p' can be removed. 'abstract_levels' is used to abort early if the algorithm is
// visiting literals at levels that cannot be removed later.
bool Solver::litRedundant(Lit p, uint32_t abstract_levels)
{
//  printf("in lit red\n");
    analyze_stack.clear(); analyze_stack.push(p);
    int top = analyze_toclear.size();
    while (analyze_stack.size() > 0){
        //assert(reason(var(analyze_stack.last())) != CRef_Undef);
		Lit k=analyze_stack.last();
		CRef tobe_check=reason(var(k));analyze_stack.pop();
		
		if(tobe_check==CRef_Pure)
		{
		  // printf("in if\n");
		   vec<int> *t=sign(k)?lit_cla0[var(k)]:lit_cla1[var(k)];
		   if(t->size()>0)
		   {
			   for (int i = 1; i < t->size(); i++){
					Lit h = clauses_data[(*t)[i]].dec_lit;
					if (!seen[var(h)] && level(var(h)) > 0){
						if (reason(var(h)) != CRef_Undef && (abstractLevel(var(h)) & abstract_levels) != 0){
							seen[var(h)] = 1;
							analyze_stack.push(h);
							analyze_toclear.push(h);
						}else{
							for (int j = top; j < analyze_toclear.size(); j++)
								seen[var(analyze_toclear[j])] = 0;
							analyze_toclear.shrink(analyze_toclear.size() - top);
							return false;
						}
					}
				}
			}
			else
			{printf("t.szie=0\n");}
			//printf("out of if\n");
		}
		else
		{
		  //  printf("in else\n");
			Clause& c = ca[tobe_check]; 
			//printf("after to be check\n");
			//if(c.learnt())
			//{printf("learn c , size():%d\n",c.size());}
			for (int i = 1; i < c.size(); i++){
				Lit pp  = c[i];
				//printf("var(pp):%d\n", var(pp));
			//	printf("literal unassign:%d\n",var(lit_Undef));
				if (!seen[var(pp)] && level(var(pp)) > 0){
					if (reason(var(pp)) != CRef_Undef && (abstractLevel(var(pp)) & abstract_levels) != 0){
						seen[var(pp)] = 1;
						analyze_stack.push(pp);
						analyze_toclear.push(pp);
					}else{
						for (int j = top; j < analyze_toclear.size(); j++)
							seen[var(analyze_toclear[j])] = 0;
						analyze_toclear.shrink(analyze_toclear.size() - top);
						return false;
					}
				}
			}
			//printf("out of else\n");
	    }
		
		
		
    }
  //printf("out of lit red\n");
    return true;

}


/*_________________________________________________________________________________________________
|
|  analyzeFinal : (p : Lit)  ->  [void]
|  
|  Description:
|    Specialized analysis procedure to express the final conflict in terms of assumptions.
|    Calculates the (possibly empty) set of assumptions that led to the assignment of 'p', and
|    stores the result in 'out_conflict'.
|________________________________________________________________________________________________@*/
void Solver::analyzeFinal(Lit p, vec<Lit>& out_conflict)
{
//printf("in analyze final\n");
    out_conflict.clear();
    out_conflict.push(p);

    if (decisionLevel() == 0)
        return;

    seen[var(p)] = 1;

    for (int i = trail.size()-1; i >= trail_lim[0]; i--){
        Var x = var(trail[i]);
        if (seen[x]){
            if (reason(x) == CRef_Undef){
                assert(level(x) > 0);
                out_conflict.push(~trail[i]);
            }else{
			    if(reason(x)==CRef_Pure)
				{
				    vec<int> *t=sign(trail[i])?lit_cla0[x]:lit_cla1[x];
					for(int j=1;j<t->size();j++)
					   {
					      Lit cj= clauses_data[(*t)[i]].dec_lit;
				          if (level(var(cj)) > 0)
							seen[var(cj)] = 1;
						  
						  
				       }
				}
				else
				{
					Clause& c = ca[reason(x)];
					for (int j = 1; j < c.size(); j++)
						if (level(var(c[j])) > 0)
							seen[var(c[j])] = 1;
				}
            }
            seen[x] = 0;
        }
    }

    seen[var(p)] = 0;
//	printf("out of analyze final\n");
}


void Solver::uncheckedEnqueue(Lit p, CRef from)
{
    assert(value(p) == l_Undef);
    assigns[var(p)] = lbool(!sign(p));
    vardata[var(p)].reason=from;
	vardata[var(p)].level= decisionLevel();
    trail.push_(p);
}


/*_________________________________________________________________________________________________
|
|  propagate : [void]  ->  [Clause*]
|  
|  Description:
|    Propagates all enqueued facts. If a conflict arises, the conflicting clause is returned,
|    otherwise CRef_Undef.
|  
|    Post-conditions:
|      * the propagation queue is empty, even if there was a conflict.
|________________________________________________________________________________________________@*/
CRef Solver::propagate(bool ini=false)
{
    CRef    confl     = CRef_Undef;
    int     num_props = 0;
    watches.cleanAll();
    
    while (qhead < trail.size()){
        Lit            p   = trail[qhead++];     // 'p' is enqueued fact to propagate.
		
        vec<Watcher>&  ws  = watches[p];
        Watcher        *i, *j, *end;
        num_props++;

        for (i = j = (Watcher*)ws, end = i + ws.size();  i != end;){
            // Try to avoid inspecting the clause:
            Lit blocker = i->blocker;
            if (value(blocker) == l_True){
                *j++ = *i++; continue; }

            // Make sure the false literal is data[1]:
            CRef     cr        = i->cref;
            Clause&  c         = ca[cr];
            Lit      false_lit = ~p;
            if (c[0] == false_lit)
                c[0] = c[1], c[1] = false_lit;
            assert(c[1] == false_lit);
            i++;

            // If 0th watch is true, then clause is already satisfied.
            Lit     first = c[0];
            Watcher w     = Watcher(cr, first);
            if (first != blocker && value(first) == l_True){
                *j++ = w; continue; }

            // Look for new watch:
            for (int k = 2; k < c.size(); k++)
                if (value(c[k]) != l_False){
                    c[1] = c[k]; c[k] = false_lit;
                    watches[~c[1]].push(w);
                    goto NextClause; }

            // Did not find watch -- clause is unit under assignment:
            *j++ = w;
            if (value(first) == l_False){
                confl = cr;
                qhead = trail.size();
                // Copy the remaining watches:
                while (i < end)
                    *j++ = *i++;
            }else{
                uncheckedEnqueue(first, cr);
                
                Clause & c = ca[cr];
                if(c.learnt()) learnt_asserting++;
            }

        NextClause:;
        }
        ws.shrink(i - j);
		if((confl  == CRef_Undef)&&(!ini)&&(slowdown_pure>restart||implement_pure))
		{newDecisionLevel_more (p);}
		//else if(confl== CRef_Undef&&(!ini)&&implement_pure)
		//{ newDecisionLevel_more (p);}      
          
    }
    propagations += num_props;
    simpDB_props -= num_props;
    
    return confl;
}


/*_________________________________________________________________________________________________
|
|  reduceDB : ()  ->  [void]
|  
|  Description:
|    Remove half of the learnt clauses, minus the clauses locked by the current assignment. Locked
|    clauses are clauses that are reason to some assignment. Binary clauses are never removed.
|________________________________________________________________________________________________@*/
struct reduceDB_lt { 
    ClauseAllocator& ca;
    reduceDB_lt(ClauseAllocator& ca_) : ca(ca_) {}
    bool operator () (CRef x, CRef y) { 
        return ca[x].size() > 2 && (ca[y].size() == 2 || ca[x].activity() < ca[y].activity()); } 
};
void Solver::reduceDB()
{
    //printf("in reduce db\n");
    int     i, j;
    double  extra_lim = cla_inc / learnts.size();    // Remove any clause below this activity

    sort(learnts, reduceDB_lt(ca));
    // Don't delete binary or locked clauses. From the rest, delete clauses from the first half
    // and clauses with activity smaller than 'extra_lim':
    for (i = j = 0; i < learnts.size(); i++){
        Clause& c = ca[learnts[i]];
        if (c.size() > 2 && !locked(c) && (i < learnts.size() / 2 || c.activity() < extra_lim))
            removeClause(learnts[i]);
        else
            learnts[j++] = learnts[i];
    }
    learnts.shrink(i - j);
    checkGarbage();
	//printf("out of reduce db\n");
	
	            int m,n;
				for( m=n=0;n<clauses_data.size();n++)
					{
					   if( !(clauses_data[n].deleted))
						{
						   clauses_data[n].cref=clauses[m];
						   m++;
						}
					}
		
}


void Solver::removeSatisfied(vec<CRef>& cs)
{
    int i, j;
    for (i = j = 0; i < cs.size(); i++){
	
	   // printf("in for\n");
        Clause& c = ca[cs[i]];
        if (satisfied(c))
            removeClause(cs[i]);
        else
            cs[j++] = cs[i];
    }
    cs.shrink(i - j);
}

void Solver::removeSatisfied_ori(vec<ClaData>& cs,bool ini)
{
  // printf("in remove sat_ori\n");
    int i, j;
    for (i = j = 0; i < cs.size(); i++){
	
	    //printf("in for\n");
		if(cs[i].deleted==false)
		{  // printf("in if\n");
		    Clause& c = ca[cs[i].cref];
			//printf("size:%d\n", c.size());
			if (satisfied(c))
				{
					
					removeClause_ori(i,ini);
				      if(!ini)
					{  cs[i].deleted=true;  }
				}
			else{j++;}
		}
    }
	//printf("out remove sat_ori\n");
	///////////////
	int m, n;
    for (m = n = 0; m < clauses.size(); m++){
	
	    //printf("in for\n");
        Clause& c = ca[clauses[m]];
        if (c.mark()==0)
          {  clauses[n++] = clauses[m];
		  }
		else   {  ca.free(clauses[m]);}
		  
    }
    clauses.shrink(m - n);
	//printf("clause.size=%d, cladata.size=%d\n",clauses.size(),j);
}

void Solver::removeClause_ori(int index,bool ini) {

    //printf("in remove cla_or\n");
	CRef cr  =clauses_data[index].cref;
    Clause& c = ca[cr];
	if(!ini&&(!clauses_data[index].deleted))
	{
	//printf("in removecla, %d\n", c.size());
	clauses_data[index].deleted=true;
	for(int j=0;j<c.size();j++)
	{
	   int m=-1;
	   
	   Lit L=c[j];
	   //printf("var(L)=%d\n",var(L));
	   vec<int> * t=sign(L)?lit_cla1[var(L)]:lit_cla0[var(L)];
	   vec<int> * f=sign(~L)?lit_cla1[var(~L)]:lit_cla0[var(~L)];
	   if(t->size()>0)
	   {
		   for(int k=0;k<(t->size());k++)
		   {
			  if((*t)[k]==index)
			  {m=k;break;}
		   }
		   if(m!=-1)
		   {
				   if(m<(t->size()-1))
				   {
					 for(int l=m;l<(t->size()-1);l++)
					  {(*t)[l]=(*t)[l+1];}
					  t->shrink(1);
				   }
				   else
					{ t->shrink(1);}
					 
					if(sign(L))
							{ if(t->size()>0)   lit_cla1_blocker[var(L)]=t->size()-1; 
							  else{
							       if(value(L)==l_Undef&&f->size()>0)
                                   {
								      uncheckedEnqueue(~L);
								      for(int k=0;k<(f->size());k++)
									  removeClause_ori((*f)[k],ini);
								   }						  
							       
							  }
							}
					 else
							{ if(t->size()>0)   lit_cla0_blocker[var(L)]=t->size()-1;
							  else{
							         if(value(L)==l_Undef&&f->size()>0)
                                   {
								      uncheckedEnqueue(~L);
								      for(int k=0;k<(f->size());k++)
									  removeClause_ori((*f)[k],ini);
								   }						  
							       
							  
							      }
							}
		   }
		}
		else
		{   if(ini==false&&value(L)==l_Undef&&f->size()>0)
		    {
			uncheckedEnqueue(~L);
			 for(int k=0;k<(f->size());k++)
			  removeClause_ori((*f)[k],ini);
			}
		}
	}
	//printf("out remove cal\n");
	}
	
    if(c.mark()==0)
		{detachClause(cr);
		// Don't leave pointers to free'd memory!
		if (locked(c)) vardata[var(c[0])].reason = CRef_Undef;
		
		c.mark(1); }
    
    
	
	//printf("out of remove cla_ori\n");
}




void Solver::rebuildOrderHeap()
{
    vec<Var> vs;
    for (Var v = 0; v < nVars(); v++)
        if (decision[v] && value(v) == l_Undef)
            vs.push(v);
    order_heap.build(vs);
}


/*_________________________________________________________________________________________________
|
|  simplify : [void]  ->  [bool]
|  
|  Description:
|    Simplify the clause database according to the current top-level assigment. Currently, the only
|    thing done here is the removal of satisfied clauses, but more things can be put here.
|________________________________________________________________________________________________@*/
bool Solver::simplify(bool  ini )
{
   
	assert(decisionLevel() == 0);
	tenth_var=nVars()/10;
	fourth_var=nVars()/4;
	half_var=nVars()/2;
       
    //printf("in simp\n");
    if (!ok || propagate(ini) != CRef_Undef)
        return ok = false;

    if (nAssigns() == simpDB_assigns || (simpDB_props > 0))
        return true;

    // Remove satisfied clauses:
	//printf("in remove sat\n");
    removeSatisfied(learnts);
	//printf("after  removing learnt\n");
	
	if(remove_satisfied)
	{  
       if(ini)	
	   removeSatisfied(clauses);
	   else
	   removeSatisfied_ori(clauses_data,ini);
	}
    if (!ok || propagate(ini) != CRef_Undef)
        return ok = false;
		
	//printf("after  removing\n");
    checkGarbage();
	if(!ini){
	            int i,j;
				for( i=j=0;j<clauses_data.size();j++)
					{
					   if( !(clauses_data[j].deleted))
						{
						   clauses_data[j].cref=clauses[i];
						   i++;
						}
					}
		}
    rebuildOrderHeap();

    simpDB_assigns = nAssigns();
    simpDB_props   = clauses_literals + learnts_literals;   // (shouldn't depend on stats really, but it will do for now)
   //  printf("after simp\n");
    return true;
}


/*_________________________________________________________________________________________________
|
|  search : (nof_conflicts : int) (params : const SearchParams&)  ->  [lbool]
|  
|  Description:
|    Search for a model the specified number of conflicts. 
|    NOTE! Use negative value for 'nof_conflicts' indicate infinity.
|  
|  Output:
|    'l_True' if a partial assigment that is consistent with respect to the clauseset is found. If
|    all variables are decision variables, this means that the clause set is satisfiable. 'l_False'
|    if the clause set is unsatisfiable. 'l_Undef' if the bound on number of conflicts is reached.
|________________________________________________________________________________________________@*/
lbool Solver::search(int nof_conflicts)
{
    assert(ok);
    int         backtrack_level;
    int         conflictC = 0;
    vec<Lit>    learnt_clause;
    starts++;

    for (;;){
	
        CRef confl = propagate(false);
        
        dump();
        
        if (confl != CRef_Undef){
            // CONFLICT
			//printf("in conflict\n");
            conflicts++; conflictC++;
            if (decisionLevel() == 0) return l_False;

            learnt_clause.clear();
			//printf("begin analyze\n");
            analyze(confl, learnt_clause, backtrack_level);
			//printf("after analyze\n");
            cancelUntil(backtrack_level);

            if (learnt_clause.size() == 1){
                uncheckedEnqueue(learnt_clause[0]);
            }else{
                CRef cr = ca.alloc(learnt_clause, true);
                if (!dis_learn) {
                    learnts.push(cr);
                    attachClause(cr);
                    learnt_clauses++;
                }
                claBumpActivity(ca[cr]);
                uncheckedEnqueue(learnt_clause[0], cr);
                if (dis_learn) {
                    ca.free(cr);
                }
            }
            
            if (output != NULL) {
              for (int i = 0; i < learnt_clause.size(); i++)
                fprintf(output, "%i " , (var(learnt_clause[i]) + 1) *
                                  (-2 * sign(learnt_clause[i]) + 1) );
              fprintf(output, "0\n");
            }

            varDecayActivity();
            claDecayActivity();

            if (--learntsize_adjust_cnt == 0){
                learntsize_adjust_confl *= learntsize_adjust_inc;
                learntsize_adjust_cnt    = (int)learntsize_adjust_confl;
                max_learnts             *= learntsize_inc;

                if (verbosity >= 1)
                   printf("c | %9d | %7d %8d %8d | %8d %8d %6.0f | %6.3f %% |\n",  
                           (int)conflicts, 
                           (int)dec_vars - (trail_lim.size() == 0 ? trail.size() : trail_lim[0]), nClauses(), (int)clauses_literals, 
                           (int)max_learnts, nLearnts(), (double)learnts_literals/nLearnts(), progressEstimate()*100);
            }
          // printf("out of conflict\n");
        }else{
            // NO CONFLICT
			//printf("in no conflict\n");
            if (nof_conflicts >= 0 && conflictC >= nof_conflicts || !withinBudget()){
                // Reached bound on number of conflicts:
                progress_estimate = progressEstimate();
                if(!dis_restart) {
                        cancelUntil(0);
                }
                return l_Undef; }

            // Simplify the set of problem clauses:
            if (decisionLevel() == 0 && !simplify(false))
                return l_False;

            if (learnts.size()-nAssigns() >= max_learnts)
                // Reduce the set of learnt clauses:
               { reduceDB();
			    // printf("in reduce\n");
			   }

            Lit next = lit_Undef;
            while (decisionLevel() < assumptions.size()){
                // Perform user provided assumption:
                Lit p = assumptions[decisionLevel()];
                if (value(p) == l_True){
                    // Dummy decision level:
                    newDecisionLevel();
				//	printf("new deci level: %d\n", decisionLevel()-1);
				 //   printf("clause trail size %d\n",clause_trail_sz);
					clause_trail_lim[decisionLevel()-1]=clause_trail_sz;
                }else if (value(p) == l_False){
                    analyzeFinal(~p, conflict);
                    return l_False;
                }else{
                    next = p;
                    break;
                }
            }

            if (next == lit_Undef){
                // New variable decision:
                decisions++;
                next = pickBranchLit();
               
                if (next == lit_Undef) {
                    // Model found:
                    return l_True;
                }
            }

            if (dump_decision) {
                if (!dump_decision_stream) {
                    dump_decision_stream = fopen(dump_decision, "wr");
                }
                fprintf(dump_decision_stream, "%d\n", var(next) + 1);
            }
            
            // Increase decision level and enqueue 'next'
            newDecisionLevel();
			//printf("new deci level: %d\n", decisionLevel()-1);
			//printf("clause trail size %d\n",clause_trail_sz);
			clause_trail_lim[decisionLevel()-1]=clause_trail_sz;
			//printf("finish deci level\n");
            uncheckedEnqueue(next);
			
			
			//printf("out of no conlfict\n");
			
			
			//
			//
			//
        }
		
    }
}


void Solver::newDecisionLevel_more (Lit p)       { 
                                                                  // find clause contains p
	
	Var k=var(p);
	//printf("in decision level\n");
	//printf("in newdecision level\n");
	vec<int> * affect_cla= (sign(p))?lit_cla1[k]:lit_cla0[k];
	int howmany_cla;
	if(affect_cla->size()>0){
	//howmany_cla=affect_cla->size();
	if(restart<slowdown_pure)
	{
		if(vardata[var(p)].level<tenth_var)
		{howmany_cla=affect_cla->size();}
		else if(vardata[var(p)].level<fourth_var)
		{howmany_cla=(affect_cla->size())>>1;}
		else if(vardata[var(p)].level<half_var)
		{howmany_cla=(affect_cla->size())>>2;}
		else
		{howmany_cla=(affect_cla->size())>>3;}
		if(howmany_cla<1)
		{howmany_cla=1;}
	}
	else if(implement_pure)
	{howmany_cla=affect_cla->size();}
	else
	{howmany_cla=1;}
	//*/
	for(int i=0;i<howmany_cla;i++)
		{	
		    int      thePos=(*affect_cla)[i];
		    ClaData& ind=clauses_data[thePos];
			
			if (ind.is_sat==false)
			{
			  // printf("%d is sat \n", thePos);
			   ind.is_sat=true;
			   ind.dec_lit=p;
			   clause_trail[clause_trail_sz++]=thePos;
			   if(ind.watch_cla_sz>0)
			   {   
			       int j,y;
				   for( y=0,j=0;j<ind.watch_cla_sz;j++)
				   {
						Lit& L =ind.watch_cla[j];
						if(value(L)==l_Undef)
						{ 
						  
									vec<int> * t=(sign(L))?lit_cla1[var(L)]:lit_cla0[var(L)];
									int blk=(sign(L))?lit_cla1_blocker[var(L)]:lit_cla0_blocker[var(L)];
								    if(blk>0)
									{
									   // printf("in blk\n");
									    if(clauses_data[(*t)[blk]].is_sat==false) 
										{
										//  printf("in blk false\n ");
										  (*t)[0]=(*t)[blk];
										  (*t)[blk]=thePos;
										  if((blk-1)==0)
										  {
										      if(sign(L))     lit_cla1_blocker[var(L)]=t->size()-1;
										      else            lit_cla0_blocker[var(L)]=t->size()-1; 
										  }
										  
										  else
										  {  if(sign(L))     lit_cla1_blocker[var(L)]=blk-1;
										     else            lit_cla0_blocker[var(L)]=blk-1; 
										  }
										  ClaData& c2=clauses_data[(*t)[0]];
										  c2.watch_cla[c2.watch_cla_sz]=L;c2.watch_cla_sz++;
										  //printf("after blk false\n");
										}
										else
										{
										   // printf("in  blk true\n");
											int ini=(blk==1)?(t->size()-1):blk-1;
											if(t->size()>2){
											for(int m=ini;m!=blk;)
										    {
											    if(clauses_data[(*t)[m]].is_sat==false)
											    {
												   (*t)[0]=(*t)[m];
												   (*t)[m]=thePos;
												  if((m-1)==0)
												  {
												     if(sign(L))     lit_cla1_blocker[var(L)]=t->size()-1;
										             else            lit_cla0_blocker[var(L)]=t->size()-1; 
												  }
												  else
												  {  if(sign(L))     lit_cla1_blocker[var(L)]=m-1;
													 else            lit_cla0_blocker[var(L)]=m-1; 
												  }
												  
												  
												  ClaData& c2=clauses_data[(*t)[0]];
										          c2.watch_cla[c2.watch_cla_sz]=L; c2.watch_cla_sz++;
												  
												  goto nextstep;
												} 
												if(m==1)
												m=t->size()-1;
												else
												m--;
											}
											}
											/*if(reason(var(p))!=CRef_Undef)
											uncheckedEnqueue(~L, reason(var(p)));
											else
											{uncheckedEnqueue(~L, CRef_Undef);}*/
											
											//printf("add L\n");
											uncheckedEnqueue(~L,CRef_Pure);
											ind.watch_cla[y]=L; y++;
											nextstep:;
											//printf("after blk true\n");
											
										}
										//printf("after blk\n");
									}
									else
									{/*if(reason(var(p))!=CRef_Undef)
									  uncheckedEnqueue(~L, reason(var(p)));
									  else
									  uncheckedEnqueue(~L, CRef_Undef);*/
									  //printf("add L\n");
									  uncheckedEnqueue(~L,CRef_Pure);
									  ind.watch_cla[y++]=L;
									}
							
						} 
						else
						{ind.watch_cla[y++]=L;}
				   }
				   ind.watch_cla_sz=y;
				 //  printf("ind wathc_cla_sz %d\n",y);
			   }
			}
		}

	}
  
		//printf("out of decision level\n");
}


double Solver::progressEstimate() const
{
    double  progress = 0;
    double  F = 1.0 / nVars();

    for (int i = 0; i <= decisionLevel(); i++){
        int beg = i == 0 ? 0 : trail_lim[i - 1];
        int end = i == decisionLevel() ? trail.size() : trail_lim[i];
        progress += pow(F, i) * (end - beg);
    }

    return progress / nVars();
}

/*
  Finite subsequences of the Luby-sequence:

  0: 1
  1: 1 1 2
  2: 1 1 2 1 1 2 4
  3: 1 1 2 1 1 2 4 1 1 2 1 1 2 4 8
  ...


 */

static double luby(double y, int x){

    // Find the finite subsequence that contains index 'x', and the
    // size of that subsequence:
    int size, seq;
    for (size = 1, seq = 0; size < x+1; seq++, size = 2*size+1);

    while (size-1 != x){
        size = (size-1)>>1;
        seq--;
        x = x % size;
    }

    return pow(y, seq);
}

// NOTE: assumptions passed in member-variable 'assumptions'.
lbool Solver::solve_()
{
   // printf("in solve\n");
    clause_trail_lim=new  int [nVars()+1];    for(int x=0;x<nVars()+1;x++) clause_trail_lim[x]=0;
	clause_trail    =new  int [nClauses()+1]; for(int y=0;y<nClauses()+1;y++) clause_trail[y]=0;
	clause_trail_sz=0;
	lit_cla0_blocker=new  int [nVars()+1];
	lit_cla1_blocker=new  int [nVars()+1];
	
    model.clear();
    conflict.clear();
    if (!ok) return l_False;

    solves++;

    max_learnts               = nClauses() * learntsize_factor;
    learntsize_adjust_confl   = learntsize_adjust_start_confl;
    learntsize_adjust_cnt     = (int)learntsize_adjust_confl;
    lbool   status            = l_Undef;
    tenth_var=nVars()/10;
	fourth_var=nVars()/4;
	half_var=nVars()/2;
	threefourth_var=3*fourth_var;
	
	clauses_data.clear();
	for(int r=0;r<clauses.size();r++)
	{    
	     Clause& cc=ca[clauses[r]];
		 if(cc.mark()==0)
		 {
	     ClaData cd= ClaData(clauses[r],cc.size());
		 clauses_data.push(cd);
		 }
	}
	
	for(int f=0;f<clauses_data.size();f++)
		{
		     CRef cr=clauses_data[f].cref;
			 Clause& C=ca[cr];
			 
			// printf("berfore for\n");
			 int z;
				for( z=0;z<C.size();z++)
				{ 
				  if(sign(C[z]))
				  {
					
				//	printf("lit cla1 size :%d   , var(c[r]) %d\n", lit_cla1.size(),(int)var(C[z]));
				lit_cla1[(int)var(C[z])]->push(f);
					}
				  else
				  {
				  //printf("lit cla0 size :%d   , var(c[r]) %d\n", lit_cla0.size(),(int)var(C[z]));
				  lit_cla0[(int)var(C[z])]->push(f);
				  }
				}
			//printf("after for\n");
		}
	
	
	
	
	//printf("%d %d\n",lit_cla1.size(), lit_cla0.size());
    for(int i=0;i<nVars();i++)
	{
       	vec<int> * t=lit_cla1[i];
	    vec<int> * f=lit_cla0[i];
		
		
		if(t->size()>0&&t!=NULL)
		{
		ClaData& c1=clauses_data[(*t)[0]]; 
		//printf("c1 watch_sz %d\n", c1.watch_cla_sz);
		 c1.watch_cla[c1.watch_cla_sz]=mkLit(i,true); c1.watch_cla_sz++;
		 
		 if(t->size()>1)
		 lit_cla1_blocker[i]=t->size()-1;
		 else
		 lit_cla1_blocker[i]=0;
		// printf("in c1\n");
		}
		if(f->size()>0&&f!=NULL)
		{
		ClaData& c0=clauses_data[(*f)[0]]; 
		//printf("c0 watch_sz %d\n", c0.watch_cla_sz);
		c0.watch_cla[c0.watch_cla_sz]=mkLit(i,false); c0.watch_cla_sz++;
		
		 if(f->size()>1)
		 lit_cla0_blocker[i]=f->size()-1;
		 else
		 lit_cla0_blocker[i]=0;
		 //printf("in c0\n");
		}
		//printf("after lit\n");
		
		/*if(t->size()>f->size())
		{  polarity[i]=true; }
		else
		{  polarity[i]=false;  }*/
		//printf("after polarity\n");
		
		//activity[i]=var_inc*(t->size()+f->size());
		//printf("after activity\n");
		//activity[i]=0;
	}
	
	//rebuildOrderHeap();
	
    if (verbosity >= 1){
        printf("c ============================[ Search Statistics ]==============================\n");
        printf("c | Conflicts |          ORIGINAL         |          LEARNT          | Progress |\n");
        printf("c |           |    Vars  Clauses Literals |    Limit  Clauses Lit/Cl |          |\n");
        printf("c ===============================================================================\n");
    }
    begin_solve=true;
    // Search:
    int curr_restarts = 0;
	int puretimes=0;
    while (status == l_Undef){
        double rest_base = luby_restart ? luby(restart_inc, curr_restarts-puretimes) : pow(restart_inc, curr_restarts-puretimes);
		if(restart<stop_pure)
        {implement_pure=false;  status = search(rest_base * restart_first);}
		else if(restart>stop_pure&&(restart-stop_pure)%freq_pure==0)
		{ implement_pure=true;  status = search((2+puretimes)* restart_first); puretimes++;}
		else
		{implement_pure=false; status = search(rest_base * restart_first);}
		
        if (!withinBudget()) break;
                if(!dis_restart) {
        curr_restarts++;restart=curr_restarts;
                }
		//if(curr_restarts>stop_pure)
		//{phase_saving=2;}
    }

    if (verbosity >= 1)
        printf("c ===============================================================================\n");


    if (status == l_True){
        // Extend & copy model:
        model.growTo(nVars());
        for (int i = 0; i < nVars(); i++) model[i] = value(i);
    }else if (status == l_False && conflict.size() == 0)
        ok = false;

    cancelUntil(0);
    return status;
}

//=================================================================================================
// Writing CNF to DIMACS:
// 
// FIXME: this needs to be rewritten completely.

static Var mapVar(Var x, vec<Var>& map, Var& max)
{
    if (map.size() <= x || map[x] == -1){
        map.growTo(x+1, -1);
        map[x] = max++;
    }
    return map[x];
}


void Solver::toDimacs(FILE* f, Clause& c, vec<Var>& map, Var& max)
{
    if (satisfied(c)) return;

    for (int i = 0; i < c.size(); i++)
        if (value(c[i]) != l_False)
            fprintf(f, "%s%d ", sign(c[i]) ? "-" : "", mapVar(var(c[i]), map, max)+1);
    fprintf(f, "0\n");
}


void Solver::toDimacs(const char *file, const vec<Lit>& assumps)
{
    FILE* f = fopen(file, "wr");
    if (f == NULL)
        fprintf(stderr, "could not open file %s\n", file), exit(1);
    toDimacs(f, assumps);
    fclose(f);
}


void Solver::toDimacs(FILE* f, const vec<Lit>& assumps)
{
    // Handle case when solver is in contradictory state:
    if (!ok){
        fprintf(f, "p cnf 1 2\n1 0\n-1 0\n");
        return; }

    vec<Var> map; Var max = 0;

    // Cannot use removeClauses here because it is not safe
    // to deallocate them at this point. Could be improved.
    int cnt = 0;
    for (int i = 0; i < clauses.size(); i++)
        if (!satisfied(ca[clauses[i]]))
            cnt++;
    for (int i = 0; i < learnts.size(); i++)
        if (!satisfied(ca[learnts[i]]))
            cnt++;
        
    for (int i = 0; i < clauses.size(); i++)
        if (!satisfied(ca[clauses[i]])){
            Clause& c = ca[clauses[i]];
            for (int j = 0; j < c.size(); j++)
                if (value(c[j]) != l_False)
                    mapVar(var(c[j]), map, max);
        }
    for (int i = 0; i < learnts.size(); i++)
        if (!satisfied(ca[learnts[i]])){
            Clause& c = ca[learnts[i]];
            for (int j = 0; j < c.size(); j++)
                if (value(c[j]) != l_False)
                    mapVar(var(c[j]), map, max);
        }

    // Assumptions are added as unit clauses:
    cnt += assumptions.size();

    fprintf(f, "p cnf %d %d\n", max, cnt);

    for (int i = 0; i < assumptions.size(); i++){
        assert(value(assumptions[i]) != l_False);
        fprintf(f, "%s%d 0\n", sign(assumptions[i]) ? "-" : "", mapVar(var(assumptions[i]), map, max)+1);
    }

    for (int i = 0; i < clauses.size(); i++)
        toDimacs(f, ca[clauses[i]], map, max);
    for (int i = 0; i < learnts.size(); i++)
        toDimacs(f, ca[learnts[i]], map, max);
}

void Solver::dump() {
    if (dump_file) {
        if (dump_counter == 0) {
            if (!dump_file_stream) {
                dump_file_stream = fopen(dump_file, "wr");
            }
            vec<Lit> as;
            toDimacs(dump_file_stream, as);
            fprintf(dump_file_stream, "$\n");
            fflush(dump_file_stream);
        }
        dump_counter++;
        if (dump_counter == dump_freq) {
            dump_counter = 0;
        }
    }
}


//=================================================================================================
// Garbage Collection methods:

void Solver::relocAll(ClauseAllocator& to)
{
    // All watchers:
    //
    // for (int i = 0; i < watches.size(); i++)
	//printf("in relocAll\n");
    watches.cleanAll();
    for (int v = 0; v < nVars(); v++)
        for (int s = 0; s < 2; s++){
            Lit p = mkLit(v, s);
            // printf(" >>> RELOCING: %s%d\n", sign(p)?"-":"", var(p)+1);
            vec<Watcher>& ws = watches[p];
            for (int j = 0; j < ws.size(); j++)
                ca.reloc(ws[j].cref, to);
        }

    // All reasons:
    //
    for (int i = 0; i < trail.size(); i++){
        Var v = var(trail[i]);

        if (reason(v) != CRef_Undef &&(reason(v) != CRef_Pure)&& (ca[reason(v)].reloced() || locked(ca[reason(v)])))
            ca.reloc(vardata[v].reason, to);
    }

    // All learnt:
    //
    for (int i = 0; i < learnts.size(); i++)
        ca.reloc(learnts[i], to);

    // All original:
    //
	
		for (int i = 0; i < clauses.size(); i++)
			{
			  if(ca[clauses[i]].mark()==0)
			  ca.reloc(clauses[i], to);
			}
		//  printf("out of relocAll\n");
		
}


void Solver::garbageCollect()
{
    // Initialize the next region to a size corresponding to the estimated utilization degree. This
    // is not precise but should avoid some unnecessary reallocations for the new region:
    ClauseAllocator to(ca.size() - ca.wasted()); 

    relocAll(to);
    if (verbosity >= 2)
        printf("c |  Garbage collection:   %12d bytes => %12d bytes             |\n", 
               ca.size()*ClauseAllocator::Unit_Size, to.size()*ClauseAllocator::Unit_Size);
    to.moveTo(ca);
}
