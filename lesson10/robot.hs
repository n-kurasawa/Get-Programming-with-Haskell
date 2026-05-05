robot (name,attack,hp) = \message -> message (name,attack,hp)

killerRobot = robot ("Kill3r",25,200)

name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,h) = h

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp
