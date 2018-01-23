-module(panel).
-compile([export_all]).


print({gotoxy,X,Y}) ->
   io:format("\e[~p;~pH",[Y,X]);
print({printxy,X,Y,Msg}) ->
   io:format("\e[~p;~pH~p",[Y,X,Msg]);   
print({clear}) ->
   io:format("\e[2J",[]);
print({tlo}) ->
  print({printxy,2,4,1.2343}),  
  io:format("",[])  .
   
printxy({X,Y,Msg}) ->
   io:format("\e[~p;~pH~p~n",[Y,X,Msg]).    
main()->
  print({clear}),
  print({printxy,1,20, "Ada"}),
  print({printxy,10,20, 2012}),
  
  print({tlo}),
  print({gotoxy,1,25}).  
  
   
      