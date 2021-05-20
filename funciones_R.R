
rcpp_foos <- 'ruta del archivo rcppfoos.cpp'

# para cargar funciones escritas en C++ utilizadas por las funciones de R
Rcpp::sourceCpp(rcpp_foos)

# ----------------------------------------------------------
# ----------------------------------------------------------


# creación de piezas

create.pieces <- function(){
   return(cbind(rep(1:2,each=8,times=1),
                rep(3:4,each=4,times=2),
                rep(5:6,each=2,times=4),
                rep(7:8,each=1,times=8)))
}

# ----------------------------------------------------------
# ----------------------------------------------------------


# partida con dos jugadores aleatorios

RAND.GAME <- function(board=array(0,c(4,4,4)),
                      player=1, # quién comienza jugando
                      positions=cbind(1:4,rep(1:4,each=4)),
                      pieces=create.pieces()){
   
   win <- 0
   for (ply in 1:15){
      if (player==1){                   # no es necesario un condicional
         choice <- sample.int(17-ply,2) # porque ambos jugadores eligen igual
      }else{                            # pero si será pertinente en otras
         choice <- sample.int(17-ply,2) # funciones
      }
      pos <- positions[choice[1],]
      piece <- pieces[choice[2],]
      positions <- positions[-choice[1],] # actualiza posiciones disponibles
      pieces <- pieces[-choice[2],] # actualiza bolsa de fichas
      
      board[pos[1],pos[2],] <- piece # coloca pieza en posición
      win <- checkboard(pos[1]-1,pos[2]-1,board) # evalúa si es movimiento ganador
      # índices comuienzan en 0
      if (win) 
         return(list(utility=player,# gana player 1 -> +1, gana player -1 -> -1
                     board=board,
                     positions_left=positions,
                     pieces_left=pieces)) 
      player <- - player # cambio de turno
   }
   # última jugada
   piece <- pieces
   pos <- positions
   board[pos[1],pos[2],] <- piece
   win <- checkboard(pos[1]-1,pos[2]-1,board) # índices comienzan en 0
   
   return(list(utility=ifelse(win,player,0), # win==1==TRUE -> player  
               board=board,              # win==0==FALSE -> 0
               positions_left=positions,
               pieces_left=pieces))
}

# ----------------------------------------------------------
# ----------------------------------------------------------


# partida con dos jugadores aleatorios y posibilidad de interrumpción en
# cualquier jugada

RAND.GAME_LIMITED <- function(board=array(0,c(4,4,4)),
                              player=1,limit,
                              positions=cbind(1:4,rep(1:4,each=4)),
                              pieces=create.pieces()){
   
   win <- 0
   for (ply in 1:limit){
      if (player==1){
         choice <- sample.int(17-ply,2)
      }else{
         choice <- sample.int(17-ply,2)
      }
      pos <- positions[choice[1],]
      piece <- pieces[choice[2],]
      positions <- positions[-choice[1],] 
      pieces <- pieces[-choice[2],] 
      
      board[pos[1],pos[2],] <- piece 
      win <- checkboard(pos[1]-1,pos[2]-1,board) 
      
      if (win) break # si se produce una victoria sal del bucle
      player <- - player 
   }
   
   # al finalizar el bucle simplemente devuelve el estado de la partida
   return(list(utility=ifelse(win,player,0),   
               board=board,              
               positions_left=positions,
               pieces_left=pieces))
}

# ----------------------------------------------------------
# ----------------------------------------------------------


# función para identificar número de fila a partir del contenido de la propia fila

selected <- function(action,actions){
   which(rowSums(actions == action[col(actions)]) == ncol(actions))
}


# ----------------------------------------------------------
# ----------------------------------------------------------

# partida con alfa-beta minimax vs. jugador aleatorio


GAME <- function(board=array(0,c(4,4,4)),
                 player=1, 
                 positions=cbind(1:4,rep(1:4,each=4)),
                 pieces=create.pieces()){
   
   win <- 0
   for (ply in 1:15){
      if (player==1){ # jugador 1 utiliza alfa-beta Minimax
         mmax <- AB_MINIMAX(board,ply,player,positions,pieces)
         if(is.null(mmax$pos)){ # si no se encuentra movimiento óptimo elige al azar
            choice <- sample.int(17-ply,2)
         }else{
            choice <- c(selected(mmax$pos,positions),selected(mmax$piece,pieces))
         }
      }else{
         choice <- sample.int(17-ply,2)
      }
      pos <- positions[choice[1],]
      piece <- pieces[choice[2],]
      positions <- positions[-choice[1],] 
      pieces <- pieces[-choice[2],] 
      
      board[pos[1],pos[2],] <- piece 
      win <- checkboard(pos[1]-1,pos[2]-1,board) 
      
      if (win) 
         return(list(utility=player,
                     board=board,
                     positions_left=positions,
                     pieces_left=pieces)) 
      player <- - player 
   }
   
   piece <- pieces
   pos <- positions
   board[pos[1],pos[2],] <- piece
   win <- checkboard(pos[1]-1,pos[2]-1,board) 
   
   return(list(utility=ifelse(win,player,0),   
               board=board,              
               positions_left=positions,
               pieces_left=pieces))
}

# ----------------------------------------------------------
# ----------------------------------------------------------

# modifica conjunto de piezas y posiciones para tener aspecto más "atractivo"

show.positions <- function(positions){
   apply(positions,1,function(r) paste0('(',r[1],',',r[2],')',
                                        collapse=''))
}

show.pieces <- function(pieces){
   apply(pieces,1,function(r) paste0(r,collapse=''))
}


# ----------------------------------------------------------
# ----------------------------------------------------------

# partida con alfa-beta minimax vs. jugador humano

GAME.INTERACTIVE <- function(board=array(0,c(4,4,4)),
                             player=1, 
                             positions=cbind(1:4,rep(1:4,each=4)),
                             pieces=create.pieces()){
   
   board2D <- matrix(0,4,4)
   win <- 0
   for (ply in 1:15){
      positions2D <- show.positions(positions)
      pieces2D <- show.pieces(pieces)
      
      if (player==1){ # jugador 1 utiliza alfa-beta Minimax
         mmax <- AB_MINIMAX(board,ply,player,positions,pieces)
         if(is.null(mmax$pos)){ 
            choice <- sample.int(17-ply,2)
         }else{
            choice <- c(selected(mmax$pos,positions),selected(mmax$piece,pieces))
         }
      }else{ # jugador humano
         # muestra estado de la partida
         print(noquote(cbind(positions2D)))
         print(noquote(cbind(pieces2D)))
         print(noquote(board2D)) 
         
         x <- menu(1:(17-ply),title='movimiento') # posiciones disponibles
         y <- menu(1:(17-ply),title='pieza') # piezas disponibles
         choice <- c(x,y)
      }
      pos <- positions[choice[1],]
      piece <- pieces[choice[2],]
      positions <- positions[-choice[1],] 
      pieces <- pieces[-choice[2],]
      
      board[pos[1],pos[2],] <- piece
      board2D[pos[1],pos[2]] <- pieces2D[choice[2]] # actualiza tablero 2D
      win <- checkboard(pos[1]-1,pos[2]-1,board) 
      
      if (win) 
         return(list(winner=player,
                     board=noquote(board2D))) 
      player <- - player 
   }
   
   piece <- pieces
   pos <- positions
   board[pos[1],pos[2],] <- piece
   board2D[pos[1],pos[2]] <- show.pieces(piece) # actualiza tablero 2D
   win <- checkboard(pos[1]-1,pos[2]-1,board) 
   
   return(list(winner=ifelse(win,player,0),  
               board=noquote(board2D)))
}
# ----------------------------------------------------------
# ----------------------------------------------------------

# Equivalencias en R

Result_R <- function(board,positions,m,pieces,p){
   pos <- positions[m,]
   piece <- pieces[p,]
   board[pos[1], pos[2],] <- piece
   return(list(board=board,
               positions=positions[-m,],
               pieces=pieces[-p,]))
}

anyrow_R <- function(b){
   for (r in 1:nrow(b)){
      if(sum(b[r,]) == 0) next
      if(length(unique(b[r,])) == 1) return(TRUE)
   }
   return(FALSE)
}

anycol_R <- function(b){
   for (c in 1:ncol(b)){
      if(sum(b[,c]) == 0) next
      if(length(unique(b[,c])) == 1) return(TRUE)
   }
   return(FALSE)
}

wind_R <- function(b){
   uniquepieces <- unique(diag(b))
   if(sum(uniquepieces) != 0 && length(uniquepieces) == 1) return(TRUE)
   return(FALSE)
}

winrevd_R <- function(b){
   uniquepieces <- unique(b[c(4,7,10,13)])
   if(sum(uniquepieces) != 0 && length(uniquepieces) == 1) return(TRUE)
   return(FALSE)
}

is_terminal_R <- function(board,ply){
   for (k in 1:4){
      b <- board[,,k]
      if (anyrow_R(b)) return(1)
      if (anycol_R(b)) return(1)
      if (wind_R(b)) return(1)
      if (winrevd_R(b)) return(1)
   }
   if (ply == 16+1) return(2) # empate al acabar la partida
   return(0)
}

payoff_R <- function(winstate,next_player,depth){
   if(winstate == 1){
      if(next_player == -1) return(10 - depth)
      return(-10 + depth)
   }
   return(0)
}

AB_MINIMAX_R <- function(board,ply,player,positions,pieces,depth=0,Alpha=-Inf,Beta=Inf){
   
   best <- list(value=NULL,pos=NULL,piece=NULL)
   terminal.state <- is_terminal_R(board,ply+depth)
   if (terminal.state || depth==4){
      best$value <- payoff_R(terminal.state,player,depth)
      return(best)
   } 
   
   if (ply+depth==16){ # requerido cuando se trata del último turno
      positions <- rbind(positions)
      pieces <- rbind(pieces) 
   }
   
   if (player==1){
      v <- -Inf
      for (m in 1:nrow(positions)){
         for (p in 1:nrow(pieces)){
            upd <- Result_R(board,positions,m,pieces,p)
            output <- AB_MINIMAX_R(upd$board,ply,-player,
                                   upd$positions,upd$pieces,
                                   depth+1,Alpha,Beta)
            if (output$value>v){
               v <- output$value
               best$value <- output$value
               best$pos <- positions[m,]
               best$piece <- pieces[p,]
               Alpha <- max(Alpha,v)
            }
            if (v>=Beta) return(best)
         }
      }
   }else{
      v <- Inf
      for (m in 1:nrow(positions)){
         for (p in 1:nrow(pieces)){
            upd <- Result_R(board,positions,m,pieces,p)
            output <- AB_MINIMAX_R(upd$board,ply,-player,
                                   upd$positions,upd$pieces,
                                   depth+1,Alpha,Beta)
            if (output$value<v){
               v <- output$value
               best$value <- output$value
               best$pos <- positions[m,]
               best$piece <- pieces[p,]
               Beta <- min(Beta,v)
            }
            if (v<=Alpha) return(best)
         }
      }
   }
   
   return(best)
}

# ----------------------------------------------------------
# ----------------------------------------------------------
