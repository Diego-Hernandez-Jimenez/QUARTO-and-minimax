#include <RcppArmadillo.h>
using namespace arma;
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]


// -----------------------------------------------------------
// -----------------------------------------------------------


// [[Rcpp::export]]
List Result(cube board, mat positions, int m, mat pieces, int p) {
   rowvec pos = positions.row(m); 
   rowvec piece = pieces.row(p); 
   board.tube(pos(0)-1,pos(1)-1) = piece; 
   positions.shed_row(m); 
   pieces.shed_row(p); 
   return List::create(Named("board") = board,
                       Named("positions") = positions, 
                       Named("pieces") = pieces);
}

// -----------------------------------------------------------
// -----------------------------------------------------------


// [[Rcpp::export]]
bool thatrow(mat b, int r) {
   rowvec uniquepieces = unique(b.row(r));
   if (sum(uniquepieces) != 0 && uniquepieces.n_elem == 1) return true;
   return false;
}

// [[Rcpp::export]]
bool thatcol(mat b, int c) {
   vec uniquepieces = unique(b.col(c));
   if (sum(uniquepieces) != 0 && uniquepieces.n_elem == 1) return true;
   return false;
}

// [[Rcpp::export]]
bool wind(mat b) {
   vec uniquepieces = unique(b.diag());
   if (sum(uniquepieces) != 0 && uniquepieces.n_elem == 1) return true;
   return false;
}

// [[Rcpp::export]]
bool winrevd(mat b) {
   vec revdiag = {b.at(3),b.at(6),b.at(9),b.at(12)};
   vec uniquepieces = unique(revdiag);
   if (sum(uniquepieces) != 0 && uniquepieces.n_elem == 1) return true;
   return false;
}

// -----------------------------------------------------------
// -----------------------------------------------------------


// [[Rcpp::export]]
int checkboard(int i, int j, cube board) {
   for (int k = 0; k < 4; ++k) {
      mat b = board.slice(k);
      if (thatrow(b,i)) return 1;
      if (thatcol(b,j)) return 1;
      if (i == j && wind(b)) return 1;
      if (i + j == 3 && winrevd(b)) return 1;
   }
   return 0;
}


// -----------------------------------------------------------
// -----------------------------------------------------------


// [[Rcpp::export]]
bool anyrow(mat b) {
   for (int r = 0; r<4; ++r) {
      if (sum(b.row(r)) == 0) continue;
      rowvec uniquepieces = unique(b.row(r));
      if (uniquepieces.n_elem == 1) return true;
   }
   return false;
}


// [[Rcpp::export]]
bool anycol(mat b) {
   for (int c = 0; c<4; ++c) {
      if (sum(b.col(c)) == 0) continue;
      vec uniquepieces = unique(b.col(c));
      if (uniquepieces.n_elem == 1) return true;
   }
   return false;
}

// -----------------------------------------------------------
// -----------------------------------------------------------


// [[Rcpp::export]]
int is_terminal(cube board, int ply) {
   for (int k=0; k<4; ++k) {
      mat b = board.slice(k);
      if (anyrow(b)) return 1;
      if (anycol(b)) return 1;
      if (wind(b)) return 1;
      if (winrevd(b)) return 1;
   }
   if (ply == 16+1) return 2; // empate si se ha realizado
   return 0;                  // la última jugada
}

// -----------------------------------------------------------
// -----------------------------------------------------------


// [[Rcpp::export]]
int payoff(int winstate, int next_player, int depth) {
   if (winstate == 1) {
      if (next_player == -1) return 10 - depth;
      return -10 + depth;
   }
   return 0;
}


// -----------------------------------------------------------
// -----------------------------------------------------------


// [[Rcpp::export]]
List AB_MINIMAX(cube board, int ply, int player, 
                mat positions, mat pieces, 
                int depth=0, int Alpha=-100, int Beta=100) {
   
   List best = List::create(Named("value") = R_NilValue,
                            Named("pos") = R_NilValue,
                            Named("piece") = R_NilValue);
   
   bool terminal_state = is_terminal(board,ply+depth);
   if (terminal_state || depth == 4) {
      int v = payoff(terminal_state,player,depth);
      best["value"] = v;
      return best;
   }
   
   if (player == 1) {
      int v = -100;
      int pos_rows = positions.n_rows;
      int piece_rows = pieces.n_rows;
      for (int m = 0; m < pos_rows; ++m) {
         for (int p = 0; p < piece_rows; ++p) {
            List upd = Result(board,positions,m,pieces,p);
            List output = AB_MINIMAX(upd["board"],ply,-player,
                                     upd["positions"],upd["pieces"],
                                     depth+1,Alpha,Beta);
            
            int mmx_value = output["value"];
            if (mmx_value > v) {
               v = mmx_value;
               best["value"] = mmx_value;
               best["pos"] = positions.row(m);
               best["piece"] = pieces.row(p);
               Alpha = std::max(Alpha,v);
            }
            if (v >= Beta) return best;
            }
         }
      
   }else {
      int v = 100;
      int pos_rows = positions.n_rows;
      int piece_rows = pieces.n_rows;
      for (int m = 0; m < pos_rows; ++m) {
         for (int p = 0; p < piece_rows; ++p) {
            List upd = Result(board,positions,m,pieces,p);
            List output = AB_MINIMAX(upd["board"],ply,-player,
                                     upd["positions"],upd["pieces"],
                                     depth+1,Alpha,Beta);
            
            int mmx_value = output["value"];
            if (mmx_value < v) {
               v = mmx_value;
               best["value"] = mmx_value;
               best["pos"] = positions.row(m);
               best["piece"] = pieces.row(p);
               Beta = std::min(Beta,v);
            }
            if (v <= Alpha) return best;
         }
      }
   }
   
   return best;
}
      
// -----------------------------------------------------------
// -----------------------------------------------------------
      