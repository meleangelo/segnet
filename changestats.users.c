/*  File src/changestats.users.c in package ergm.userterms, part of the Statnet suite
 *  of packages for network analysis, http://statnet.org .
 *
 *  This software is distributed under the GPL-3 license.  It is free,
 *  open source, and has the attribution requirements (GPL Section 7) at
 *  http://statnet.org/attribution
 *
 *  Copyright 2012-2018 Statnet Commons
 */
#include "changestats.users.h"

CHANGESTAT_FN(d_mindegree) {
  Vertex t, h, node3;
  int i, mindeg, hdeg, tdeg;
  Edge e;
  int attrflag;
  double t_nodecov, h_nodecov;

  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    t = TAIL(i); h = HEAD(i);
    attrflag = INPUT_PARAM[0];
    mindeg = INPUT_PARAM[1];
    if(attrflag==0){
      tdeg = IN_DEG[t]+OUT_DEG[t];
      hdeg = IN_DEG[h]+OUT_DEG[h];
      CHANGE_STAT[0] += IS_UNDIRECTED_EDGE(t,h) ?
        - (tdeg==mindeg) - (hdeg==mindeg) :
        (tdeg==mindeg-1) + (hdeg==mindeg-1);
    }else{
      t_nodecov = INPUT_PARAM[t+1];
      h_nodecov = INPUT_PARAM[h+1];
      if (t_nodecov == h_nodecov) {
        tdeg = 0;
        STEP_THROUGH_OUTEDGES(t, e, node3) { /* step through outedges of tail */
          if(INPUT_PARAM[node3+1]==t_nodecov){++tdeg;}
        }
        STEP_THROUGH_INEDGES(t, e, node3) { /* step through inedges of tail */
          if(INPUT_PARAM[node3+1]==t_nodecov){++tdeg;}
        }
        hdeg = 0;
        STEP_THROUGH_OUTEDGES(h, e, node3) { /* step through outedges of head */
          if(INPUT_PARAM[node3+1]==h_nodecov){++hdeg;}
        }
        STEP_THROUGH_INEDGES(h, e, node3) { /* step through inedges of head */
          if(INPUT_PARAM[node3+1]==h_nodecov){++hdeg;}
        }
        CHANGE_STAT[0] += IS_UNDIRECTED_EDGE(t,h) ?
          - (tdeg==mindeg) - (hdeg==mindeg) :
          (tdeg==mindeg-1) + (hdeg==mindeg-1);
      }else{
        CHANGE_STAT[0] = 0;
      }
    }
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}








/*****************
 changestat: d_indhomophily
*****************/
D_CHANGESTAT_FN(d_indhomophily) { 
  Edge e;
  Vertex tail, head, change, node3;
  int i, j;
  double tailattr, headattr, node3attr, edgemult;
  
  /* *** don't forget tail -> head */    
  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    tail = TAIL(i);
    head = HEAD(i);
    edgemult = IS_OUTEDGE(tail, head) ? -1.0 : 1.0;
    change = 0.0;
    if(N_INPUT_PARAMS > 0){ /* match on attributes */
      tailattr = INPUT_ATTRIB[tail-1];
      headattr = INPUT_ATTRIB[head-1];
        STEP_THROUGH_OUTEDGES(head, e, node3) { /* step through outedges of head */
          if ((node3 != tail) && (node3 != head)) { /* consider only when the node is different from tail and head*/
            node3attr = INPUT_ATTRIB[node3-1]; /*attribute od node3*/
            if(tailattr == node3attr) { /* if tail has same attr as node3 then change stats*/
              change += IS_OUTEDGE(head, node3);
            }
            
          }
            
        }

       if(N_CHANGE_STATS > 1) { /* diff = TRUE; matches must be tabled */
          for (j=0; j<N_CHANGE_STATS; j++) {
            if(tailattr == INPUT_PARAM[j]){
              CHANGE_STAT[j] += edgemult * change;
            }else{
              CHANGE_STAT[j] += 0.0;
            } 
                         
          }
        } else { /* diff = FALSE; all matches equivalent */
          CHANGE_STAT[0] += edgemult * change;          
        }


        change = 0.0;
        STEP_THROUGH_INEDGES(tail, e, node3) { /* step through inedges of head */
          if ((node3 != tail) && (node3 != head)) {
            node3attr = INPUT_ATTRIB[node3-1];
            if(headattr == node3attr) {
              change += IS_INEDGE(tail, node3);
            }
            
          }
            
        }
        
        if(N_CHANGE_STATS > 1) { /* diff = TRUE; matches must be tabled */
          for (j=0; j<N_CHANGE_STATS; j++) {
            if(headattr == INPUT_PARAM[j]){
              CHANGE_STAT[j] += edgemult * change;
            }else{
              CHANGE_STAT[j] += 0.0;
            } 
                         
          }
        } else { /* diff = FALSE; all matches equivalent */
          CHANGE_STAT[0] += edgemult * change;          
        }
      
    }else{ /* no attribute matching */
      STEP_THROUGH_OUTEDGES(head, e, node3) { /* step through outedges of head */
        if ((node3 != head) && (node3 != tail)) {
          change += IS_OUTEDGE(head, node3);
        }
      }
      STEP_THROUGH_INEDGES(tail, e, node3) { /* step through inedges of head */
        if ((node3 != head) && (node3 != tail)) {
          change += IS_INEDGE(tail, node3);
        }
      }
      CHANGE_STAT[0] += edgemult * change;
    }
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}



/*****************
 changestat: d_indhomophily
*****************/
D_CHANGESTAT_FN(d_indhomophily_old) { 
  Edge e;
  Vertex tail, head, change, node3;
  int i, j;
  double tailattr, headattr, node3attr, edgemult;
  
  /* *** don't forget tail -> head */    
  ZERO_ALL_CHANGESTATS(i);
  FOR_EACH_TOGGLE(i) {
    tail = TAIL(i);
    head = HEAD(i);
    edgemult = IS_OUTEDGE(tail, head) ? -1.0 : 1.0;
    change = 0.0;
    if(N_INPUT_PARAMS > 0){ /* match on attributes */
      tailattr = INPUT_ATTRIB[tail-1];
      headattr = INPUT_ATTRIB[head-1];
        STEP_THROUGH_OUTEDGES(head, e, node3) { /* step through outedges of head */
          if ((node3 != tail) && (node3 != head)) { /* consider only when the node is different from tail and head*/
            node3attr = INPUT_ATTRIB[node3-1]; /*attribute od node3*/
            if(tailattr == node3attr) { /* if tail has same attr as node3 then change stats*/
              change += IS_OUTEDGE(head, node3)*(tailattr==node3attr);
            }
            
          }
            
        }
        STEP_THROUGH_INEDGES(tail, e, node3) { /* step through inedges of head */
          if ((node3 != tail) && (node3 != head)) {
            node3attr = INPUT_ATTRIB[node3-1];
            if(headattr == node3attr) {
              change += IS_INEDGE(tail, node3)*(headattr==node3attr);
            }
            
          }
            
        }
        
        if(N_CHANGE_STATS > 1) { /* diff = TRUE; matches must be tabled */
          for (j=0; j<N_CHANGE_STATS; j++) {
            if(tailattr == INPUT_PARAM[j]){
              CHANGE_STAT[j] += edgemult * change;
            }else{
              CHANGE_STAT[j] += 0.0;
            } 
                         
          }
        } else { /* diff = FALSE; all matches equivalent */
          CHANGE_STAT[0] += edgemult * change;          
        }
      
    }else{ /* no attribute matching */
      STEP_THROUGH_OUTEDGES(head, e, node3) { /* step through outedges of head */
        if ((node3 != head) && (node3 != tail)) {
          change += IS_OUTEDGE(head, node3);
        }
      }
      STEP_THROUGH_INEDGES(tail, e, node3) { /* step through inedges of head */
        if ((node3 != head) && (node3 != tail)) {
          change += IS_INEDGE(tail, node3);
        }
      }
      CHANGE_STAT[0] += edgemult * change;
    }
    TOGGLE_IF_MORE_TO_COME(i);
  }
  UNDO_PREVIOUS_TOGGLES(i);
}
