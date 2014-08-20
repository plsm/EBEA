/**
 * Provides a predicate that encodes a strategy with a history of size one.

 * @author Pedro Mariano
 * @version 1.0 2014/02/07
 */
:- module gl.givetake.strategy.encodecode.

:- interface.

:- pred encodecodeHist(givetake.strategy.strategy, int).
:- mode encodecodeHist( in(history), out) is det.
:- mode encodecodeHist(out(history),  in) is semidet.

:- implementation.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of exported types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of private types

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of exported predicates and functions

encodecodeHist(history( no, no, no, no, no, no, no, no, no, no),0).
encodecodeHist(history(yes, no, no, no, no, no, no, no, no, no),1).
encodecodeHist(history( no,yes, no, no, no, no, no, no, no, no),2).
encodecodeHist(history(yes,yes, no, no, no, no, no, no, no, no),3).
encodecodeHist(history( no, no,yes, no, no, no, no, no, no, no),4).
encodecodeHist(history(yes, no,yes, no, no, no, no, no, no, no),5).
encodecodeHist(history( no,yes,yes, no, no, no, no, no, no, no),6).
encodecodeHist(history(yes,yes,yes, no, no, no, no, no, no, no),7).
encodecodeHist(history( no, no, no,yes, no, no, no, no, no, no),8).
encodecodeHist(history(yes, no, no,yes, no, no, no, no, no, no),9).
encodecodeHist(history( no,yes, no,yes, no, no, no, no, no, no),10).
encodecodeHist(history(yes,yes, no,yes, no, no, no, no, no, no),11).
encodecodeHist(history( no, no,yes,yes, no, no, no, no, no, no),12).
encodecodeHist(history(yes, no,yes,yes, no, no, no, no, no, no),13).
encodecodeHist(history( no,yes,yes,yes, no, no, no, no, no, no),14).
encodecodeHist(history(yes,yes,yes,yes, no, no, no, no, no, no),15).
encodecodeHist(history( no, no, no, no,yes, no, no, no, no, no),16).
encodecodeHist(history(yes, no, no, no,yes, no, no, no, no, no),17).
encodecodeHist(history( no,yes, no, no,yes, no, no, no, no, no),18).
encodecodeHist(history(yes,yes, no, no,yes, no, no, no, no, no),19).
encodecodeHist(history( no, no,yes, no,yes, no, no, no, no, no),20).
encodecodeHist(history(yes, no,yes, no,yes, no, no, no, no, no),21).
encodecodeHist(history( no,yes,yes, no,yes, no, no, no, no, no),22).
encodecodeHist(history(yes,yes,yes, no,yes, no, no, no, no, no),23).
encodecodeHist(history( no, no, no,yes,yes, no, no, no, no, no),24).
encodecodeHist(history(yes, no, no,yes,yes, no, no, no, no, no),25).
encodecodeHist(history( no,yes, no,yes,yes, no, no, no, no, no),26).
encodecodeHist(history(yes,yes, no,yes,yes, no, no, no, no, no),27).
encodecodeHist(history( no, no,yes,yes,yes, no, no, no, no, no),28).
encodecodeHist(history(yes, no,yes,yes,yes, no, no, no, no, no),29).
encodecodeHist(history( no,yes,yes,yes,yes, no, no, no, no, no),30).
encodecodeHist(history(yes,yes,yes,yes,yes, no, no, no, no, no),31).
encodecodeHist(history( no, no, no, no, no,yes, no, no, no, no),32).
encodecodeHist(history(yes, no, no, no, no,yes, no, no, no, no),33).
encodecodeHist(history( no,yes, no, no, no,yes, no, no, no, no),34).
encodecodeHist(history(yes,yes, no, no, no,yes, no, no, no, no),35).
encodecodeHist(history( no, no,yes, no, no,yes, no, no, no, no),36).
encodecodeHist(history(yes, no,yes, no, no,yes, no, no, no, no),37).
encodecodeHist(history( no,yes,yes, no, no,yes, no, no, no, no),38).
encodecodeHist(history(yes,yes,yes, no, no,yes, no, no, no, no),39).
encodecodeHist(history( no, no, no,yes, no,yes, no, no, no, no),40).
encodecodeHist(history(yes, no, no,yes, no,yes, no, no, no, no),41).
encodecodeHist(history( no,yes, no,yes, no,yes, no, no, no, no),42).
encodecodeHist(history(yes,yes, no,yes, no,yes, no, no, no, no),43).
encodecodeHist(history( no, no,yes,yes, no,yes, no, no, no, no),44).
encodecodeHist(history(yes, no,yes,yes, no,yes, no, no, no, no),45).
encodecodeHist(history( no,yes,yes,yes, no,yes, no, no, no, no),46).
encodecodeHist(history(yes,yes,yes,yes, no,yes, no, no, no, no),47).
encodecodeHist(history( no, no, no, no,yes,yes, no, no, no, no),48).
encodecodeHist(history(yes, no, no, no,yes,yes, no, no, no, no),49).
encodecodeHist(history( no,yes, no, no,yes,yes, no, no, no, no),50).
encodecodeHist(history(yes,yes, no, no,yes,yes, no, no, no, no),51).
encodecodeHist(history( no, no,yes, no,yes,yes, no, no, no, no),52).
encodecodeHist(history(yes, no,yes, no,yes,yes, no, no, no, no),53).
encodecodeHist(history( no,yes,yes, no,yes,yes, no, no, no, no),54).
encodecodeHist(history(yes,yes,yes, no,yes,yes, no, no, no, no),55).
encodecodeHist(history( no, no, no,yes,yes,yes, no, no, no, no),56).
encodecodeHist(history(yes, no, no,yes,yes,yes, no, no, no, no),57).
encodecodeHist(history( no,yes, no,yes,yes,yes, no, no, no, no),58).
encodecodeHist(history(yes,yes, no,yes,yes,yes, no, no, no, no),59).
encodecodeHist(history( no, no,yes,yes,yes,yes, no, no, no, no),60).
encodecodeHist(history(yes, no,yes,yes,yes,yes, no, no, no, no),61).
encodecodeHist(history( no,yes,yes,yes,yes,yes, no, no, no, no),62).
encodecodeHist(history(yes,yes,yes,yes,yes,yes, no, no, no, no),63).
encodecodeHist(history( no, no, no, no, no, no,yes, no, no, no),64).
encodecodeHist(history(yes, no, no, no, no, no,yes, no, no, no),65).
encodecodeHist(history( no,yes, no, no, no, no,yes, no, no, no),66).
encodecodeHist(history(yes,yes, no, no, no, no,yes, no, no, no),67).
encodecodeHist(history( no, no,yes, no, no, no,yes, no, no, no),68).
encodecodeHist(history(yes, no,yes, no, no, no,yes, no, no, no),69).
encodecodeHist(history( no,yes,yes, no, no, no,yes, no, no, no),70).
encodecodeHist(history(yes,yes,yes, no, no, no,yes, no, no, no),71).
encodecodeHist(history( no, no, no,yes, no, no,yes, no, no, no),72).
encodecodeHist(history(yes, no, no,yes, no, no,yes, no, no, no),73).
encodecodeHist(history( no,yes, no,yes, no, no,yes, no, no, no),74).
encodecodeHist(history(yes,yes, no,yes, no, no,yes, no, no, no),75).
encodecodeHist(history( no, no,yes,yes, no, no,yes, no, no, no),76).
encodecodeHist(history(yes, no,yes,yes, no, no,yes, no, no, no),77).
encodecodeHist(history( no,yes,yes,yes, no, no,yes, no, no, no),78).
encodecodeHist(history(yes,yes,yes,yes, no, no,yes, no, no, no),79).
encodecodeHist(history( no, no, no, no,yes, no,yes, no, no, no),80).
encodecodeHist(history(yes, no, no, no,yes, no,yes, no, no, no),81).
encodecodeHist(history( no,yes, no, no,yes, no,yes, no, no, no),82).
encodecodeHist(history(yes,yes, no, no,yes, no,yes, no, no, no),83).
encodecodeHist(history( no, no,yes, no,yes, no,yes, no, no, no),84).
encodecodeHist(history(yes, no,yes, no,yes, no,yes, no, no, no),85).
encodecodeHist(history( no,yes,yes, no,yes, no,yes, no, no, no),86).
encodecodeHist(history(yes,yes,yes, no,yes, no,yes, no, no, no),87).
encodecodeHist(history( no, no, no,yes,yes, no,yes, no, no, no),88).
encodecodeHist(history(yes, no, no,yes,yes, no,yes, no, no, no),89).
encodecodeHist(history( no,yes, no,yes,yes, no,yes, no, no, no),90).
encodecodeHist(history(yes,yes, no,yes,yes, no,yes, no, no, no),91).
encodecodeHist(history( no, no,yes,yes,yes, no,yes, no, no, no),92).
encodecodeHist(history(yes, no,yes,yes,yes, no,yes, no, no, no),93).
encodecodeHist(history( no,yes,yes,yes,yes, no,yes, no, no, no),94).
encodecodeHist(history(yes,yes,yes,yes,yes, no,yes, no, no, no),95).
encodecodeHist(history( no, no, no, no, no,yes,yes, no, no, no),96).
encodecodeHist(history(yes, no, no, no, no,yes,yes, no, no, no),97).
encodecodeHist(history( no,yes, no, no, no,yes,yes, no, no, no),98).
encodecodeHist(history(yes,yes, no, no, no,yes,yes, no, no, no),99).
encodecodeHist(history( no, no,yes, no, no,yes,yes, no, no, no),100).
encodecodeHist(history(yes, no,yes, no, no,yes,yes, no, no, no),101).
encodecodeHist(history( no,yes,yes, no, no,yes,yes, no, no, no),102).
encodecodeHist(history(yes,yes,yes, no, no,yes,yes, no, no, no),103).
encodecodeHist(history( no, no, no,yes, no,yes,yes, no, no, no),104).
encodecodeHist(history(yes, no, no,yes, no,yes,yes, no, no, no),105).
encodecodeHist(history( no,yes, no,yes, no,yes,yes, no, no, no),106).
encodecodeHist(history(yes,yes, no,yes, no,yes,yes, no, no, no),107).
encodecodeHist(history( no, no,yes,yes, no,yes,yes, no, no, no),108).
encodecodeHist(history(yes, no,yes,yes, no,yes,yes, no, no, no),109).
encodecodeHist(history( no,yes,yes,yes, no,yes,yes, no, no, no),110).
encodecodeHist(history(yes,yes,yes,yes, no,yes,yes, no, no, no),111).
encodecodeHist(history( no, no, no, no,yes,yes,yes, no, no, no),112).
encodecodeHist(history(yes, no, no, no,yes,yes,yes, no, no, no),113).
encodecodeHist(history( no,yes, no, no,yes,yes,yes, no, no, no),114).
encodecodeHist(history(yes,yes, no, no,yes,yes,yes, no, no, no),115).
encodecodeHist(history( no, no,yes, no,yes,yes,yes, no, no, no),116).
encodecodeHist(history(yes, no,yes, no,yes,yes,yes, no, no, no),117).
encodecodeHist(history( no,yes,yes, no,yes,yes,yes, no, no, no),118).
encodecodeHist(history(yes,yes,yes, no,yes,yes,yes, no, no, no),119).
encodecodeHist(history( no, no, no,yes,yes,yes,yes, no, no, no),120).
encodecodeHist(history(yes, no, no,yes,yes,yes,yes, no, no, no),121).
encodecodeHist(history( no,yes, no,yes,yes,yes,yes, no, no, no),122).
encodecodeHist(history(yes,yes, no,yes,yes,yes,yes, no, no, no),123).
encodecodeHist(history( no, no,yes,yes,yes,yes,yes, no, no, no),124).
encodecodeHist(history(yes, no,yes,yes,yes,yes,yes, no, no, no),125).
encodecodeHist(history( no,yes,yes,yes,yes,yes,yes, no, no, no),126).
encodecodeHist(history(yes,yes,yes,yes,yes,yes,yes, no, no, no),127).
encodecodeHist(history( no, no, no, no, no, no, no,yes, no, no),128).
encodecodeHist(history(yes, no, no, no, no, no, no,yes, no, no),129).
encodecodeHist(history( no,yes, no, no, no, no, no,yes, no, no),130).
encodecodeHist(history(yes,yes, no, no, no, no, no,yes, no, no),131).
encodecodeHist(history( no, no,yes, no, no, no, no,yes, no, no),132).
encodecodeHist(history(yes, no,yes, no, no, no, no,yes, no, no),133).
encodecodeHist(history( no,yes,yes, no, no, no, no,yes, no, no),134).
encodecodeHist(history(yes,yes,yes, no, no, no, no,yes, no, no),135).
encodecodeHist(history( no, no, no,yes, no, no, no,yes, no, no),136).
encodecodeHist(history(yes, no, no,yes, no, no, no,yes, no, no),137).
encodecodeHist(history( no,yes, no,yes, no, no, no,yes, no, no),138).
encodecodeHist(history(yes,yes, no,yes, no, no, no,yes, no, no),139).
encodecodeHist(history( no, no,yes,yes, no, no, no,yes, no, no),140).
encodecodeHist(history(yes, no,yes,yes, no, no, no,yes, no, no),141).
encodecodeHist(history( no,yes,yes,yes, no, no, no,yes, no, no),142).
encodecodeHist(history(yes,yes,yes,yes, no, no, no,yes, no, no),143).
encodecodeHist(history( no, no, no, no,yes, no, no,yes, no, no),144).
encodecodeHist(history(yes, no, no, no,yes, no, no,yes, no, no),145).
encodecodeHist(history( no,yes, no, no,yes, no, no,yes, no, no),146).
encodecodeHist(history(yes,yes, no, no,yes, no, no,yes, no, no),147).
encodecodeHist(history( no, no,yes, no,yes, no, no,yes, no, no),148).
encodecodeHist(history(yes, no,yes, no,yes, no, no,yes, no, no),149).
encodecodeHist(history( no,yes,yes, no,yes, no, no,yes, no, no),150).
encodecodeHist(history(yes,yes,yes, no,yes, no, no,yes, no, no),151).
encodecodeHist(history( no, no, no,yes,yes, no, no,yes, no, no),152).
encodecodeHist(history(yes, no, no,yes,yes, no, no,yes, no, no),153).
encodecodeHist(history( no,yes, no,yes,yes, no, no,yes, no, no),154).
encodecodeHist(history(yes,yes, no,yes,yes, no, no,yes, no, no),155).
encodecodeHist(history( no, no,yes,yes,yes, no, no,yes, no, no),156).
encodecodeHist(history(yes, no,yes,yes,yes, no, no,yes, no, no),157).
encodecodeHist(history( no,yes,yes,yes,yes, no, no,yes, no, no),158).
encodecodeHist(history(yes,yes,yes,yes,yes, no, no,yes, no, no),159).
encodecodeHist(history( no, no, no, no, no,yes, no,yes, no, no),160).
encodecodeHist(history(yes, no, no, no, no,yes, no,yes, no, no),161).
encodecodeHist(history( no,yes, no, no, no,yes, no,yes, no, no),162).
encodecodeHist(history(yes,yes, no, no, no,yes, no,yes, no, no),163).
encodecodeHist(history( no, no,yes, no, no,yes, no,yes, no, no),164).
encodecodeHist(history(yes, no,yes, no, no,yes, no,yes, no, no),165).
encodecodeHist(history( no,yes,yes, no, no,yes, no,yes, no, no),166).
encodecodeHist(history(yes,yes,yes, no, no,yes, no,yes, no, no),167).
encodecodeHist(history( no, no, no,yes, no,yes, no,yes, no, no),168).
encodecodeHist(history(yes, no, no,yes, no,yes, no,yes, no, no),169).
encodecodeHist(history( no,yes, no,yes, no,yes, no,yes, no, no),170).
encodecodeHist(history(yes,yes, no,yes, no,yes, no,yes, no, no),171).
encodecodeHist(history( no, no,yes,yes, no,yes, no,yes, no, no),172).
encodecodeHist(history(yes, no,yes,yes, no,yes, no,yes, no, no),173).
encodecodeHist(history( no,yes,yes,yes, no,yes, no,yes, no, no),174).
encodecodeHist(history(yes,yes,yes,yes, no,yes, no,yes, no, no),175).
encodecodeHist(history( no, no, no, no,yes,yes, no,yes, no, no),176).
encodecodeHist(history(yes, no, no, no,yes,yes, no,yes, no, no),177).
encodecodeHist(history( no,yes, no, no,yes,yes, no,yes, no, no),178).
encodecodeHist(history(yes,yes, no, no,yes,yes, no,yes, no, no),179).
encodecodeHist(history( no, no,yes, no,yes,yes, no,yes, no, no),180).
encodecodeHist(history(yes, no,yes, no,yes,yes, no,yes, no, no),181).
encodecodeHist(history( no,yes,yes, no,yes,yes, no,yes, no, no),182).
encodecodeHist(history(yes,yes,yes, no,yes,yes, no,yes, no, no),183).
encodecodeHist(history( no, no, no,yes,yes,yes, no,yes, no, no),184).
encodecodeHist(history(yes, no, no,yes,yes,yes, no,yes, no, no),185).
encodecodeHist(history( no,yes, no,yes,yes,yes, no,yes, no, no),186).
encodecodeHist(history(yes,yes, no,yes,yes,yes, no,yes, no, no),187).
encodecodeHist(history( no, no,yes,yes,yes,yes, no,yes, no, no),188).
encodecodeHist(history(yes, no,yes,yes,yes,yes, no,yes, no, no),189).
encodecodeHist(history( no,yes,yes,yes,yes,yes, no,yes, no, no),190).
encodecodeHist(history(yes,yes,yes,yes,yes,yes, no,yes, no, no),191).
encodecodeHist(history( no, no, no, no, no, no,yes,yes, no, no),192).
encodecodeHist(history(yes, no, no, no, no, no,yes,yes, no, no),193).
encodecodeHist(history( no,yes, no, no, no, no,yes,yes, no, no),194).
encodecodeHist(history(yes,yes, no, no, no, no,yes,yes, no, no),195).
encodecodeHist(history( no, no,yes, no, no, no,yes,yes, no, no),196).
encodecodeHist(history(yes, no,yes, no, no, no,yes,yes, no, no),197).
encodecodeHist(history( no,yes,yes, no, no, no,yes,yes, no, no),198).
encodecodeHist(history(yes,yes,yes, no, no, no,yes,yes, no, no),199).
encodecodeHist(history( no, no, no,yes, no, no,yes,yes, no, no),200).
encodecodeHist(history(yes, no, no,yes, no, no,yes,yes, no, no),201).
encodecodeHist(history( no,yes, no,yes, no, no,yes,yes, no, no),202).
encodecodeHist(history(yes,yes, no,yes, no, no,yes,yes, no, no),203).
encodecodeHist(history( no, no,yes,yes, no, no,yes,yes, no, no),204).
encodecodeHist(history(yes, no,yes,yes, no, no,yes,yes, no, no),205).
encodecodeHist(history( no,yes,yes,yes, no, no,yes,yes, no, no),206).
encodecodeHist(history(yes,yes,yes,yes, no, no,yes,yes, no, no),207).
encodecodeHist(history( no, no, no, no,yes, no,yes,yes, no, no),208).
encodecodeHist(history(yes, no, no, no,yes, no,yes,yes, no, no),209).
encodecodeHist(history( no,yes, no, no,yes, no,yes,yes, no, no),210).
encodecodeHist(history(yes,yes, no, no,yes, no,yes,yes, no, no),211).
encodecodeHist(history( no, no,yes, no,yes, no,yes,yes, no, no),212).
encodecodeHist(history(yes, no,yes, no,yes, no,yes,yes, no, no),213).
encodecodeHist(history( no,yes,yes, no,yes, no,yes,yes, no, no),214).
encodecodeHist(history(yes,yes,yes, no,yes, no,yes,yes, no, no),215).
encodecodeHist(history( no, no, no,yes,yes, no,yes,yes, no, no),216).
encodecodeHist(history(yes, no, no,yes,yes, no,yes,yes, no, no),217).
encodecodeHist(history( no,yes, no,yes,yes, no,yes,yes, no, no),218).
encodecodeHist(history(yes,yes, no,yes,yes, no,yes,yes, no, no),219).
encodecodeHist(history( no, no,yes,yes,yes, no,yes,yes, no, no),220).
encodecodeHist(history(yes, no,yes,yes,yes, no,yes,yes, no, no),221).
encodecodeHist(history( no,yes,yes,yes,yes, no,yes,yes, no, no),222).
encodecodeHist(history(yes,yes,yes,yes,yes, no,yes,yes, no, no),223).
encodecodeHist(history( no, no, no, no, no,yes,yes,yes, no, no),224).
encodecodeHist(history(yes, no, no, no, no,yes,yes,yes, no, no),225).
encodecodeHist(history( no,yes, no, no, no,yes,yes,yes, no, no),226).
encodecodeHist(history(yes,yes, no, no, no,yes,yes,yes, no, no),227).
encodecodeHist(history( no, no,yes, no, no,yes,yes,yes, no, no),228).
encodecodeHist(history(yes, no,yes, no, no,yes,yes,yes, no, no),229).
encodecodeHist(history( no,yes,yes, no, no,yes,yes,yes, no, no),230).
encodecodeHist(history(yes,yes,yes, no, no,yes,yes,yes, no, no),231).
encodecodeHist(history( no, no, no,yes, no,yes,yes,yes, no, no),232).
encodecodeHist(history(yes, no, no,yes, no,yes,yes,yes, no, no),233).
encodecodeHist(history( no,yes, no,yes, no,yes,yes,yes, no, no),234).
encodecodeHist(history(yes,yes, no,yes, no,yes,yes,yes, no, no),235).
encodecodeHist(history( no, no,yes,yes, no,yes,yes,yes, no, no),236).
encodecodeHist(history(yes, no,yes,yes, no,yes,yes,yes, no, no),237).
encodecodeHist(history( no,yes,yes,yes, no,yes,yes,yes, no, no),238).
encodecodeHist(history(yes,yes,yes,yes, no,yes,yes,yes, no, no),239).
encodecodeHist(history( no, no, no, no,yes,yes,yes,yes, no, no),240).
encodecodeHist(history(yes, no, no, no,yes,yes,yes,yes, no, no),241).
encodecodeHist(history( no,yes, no, no,yes,yes,yes,yes, no, no),242).
encodecodeHist(history(yes,yes, no, no,yes,yes,yes,yes, no, no),243).
encodecodeHist(history( no, no,yes, no,yes,yes,yes,yes, no, no),244).
encodecodeHist(history(yes, no,yes, no,yes,yes,yes,yes, no, no),245).
encodecodeHist(history( no,yes,yes, no,yes,yes,yes,yes, no, no),246).
encodecodeHist(history(yes,yes,yes, no,yes,yes,yes,yes, no, no),247).
encodecodeHist(history( no, no, no,yes,yes,yes,yes,yes, no, no),248).
encodecodeHist(history(yes, no, no,yes,yes,yes,yes,yes, no, no),249).
encodecodeHist(history( no,yes, no,yes,yes,yes,yes,yes, no, no),250).
encodecodeHist(history(yes,yes, no,yes,yes,yes,yes,yes, no, no),251).
encodecodeHist(history( no, no,yes,yes,yes,yes,yes,yes, no, no),252).
encodecodeHist(history(yes, no,yes,yes,yes,yes,yes,yes, no, no),253).
encodecodeHist(history( no,yes,yes,yes,yes,yes,yes,yes, no, no),254).
encodecodeHist(history(yes,yes,yes,yes,yes,yes,yes,yes, no, no),255).
encodecodeHist(history( no, no, no, no, no, no, no, no,yes, no),256).
encodecodeHist(history(yes, no, no, no, no, no, no, no,yes, no),257).
encodecodeHist(history( no,yes, no, no, no, no, no, no,yes, no),258).
encodecodeHist(history(yes,yes, no, no, no, no, no, no,yes, no),259).
encodecodeHist(history( no, no,yes, no, no, no, no, no,yes, no),260).
encodecodeHist(history(yes, no,yes, no, no, no, no, no,yes, no),261).
encodecodeHist(history( no,yes,yes, no, no, no, no, no,yes, no),262).
encodecodeHist(history(yes,yes,yes, no, no, no, no, no,yes, no),263).
encodecodeHist(history( no, no, no,yes, no, no, no, no,yes, no),264).
encodecodeHist(history(yes, no, no,yes, no, no, no, no,yes, no),265).
encodecodeHist(history( no,yes, no,yes, no, no, no, no,yes, no),266).
encodecodeHist(history(yes,yes, no,yes, no, no, no, no,yes, no),267).
encodecodeHist(history( no, no,yes,yes, no, no, no, no,yes, no),268).
encodecodeHist(history(yes, no,yes,yes, no, no, no, no,yes, no),269).
encodecodeHist(history( no,yes,yes,yes, no, no, no, no,yes, no),270).
encodecodeHist(history(yes,yes,yes,yes, no, no, no, no,yes, no),271).
encodecodeHist(history( no, no, no, no,yes, no, no, no,yes, no),272).
encodecodeHist(history(yes, no, no, no,yes, no, no, no,yes, no),273).
encodecodeHist(history( no,yes, no, no,yes, no, no, no,yes, no),274).
encodecodeHist(history(yes,yes, no, no,yes, no, no, no,yes, no),275).
encodecodeHist(history( no, no,yes, no,yes, no, no, no,yes, no),276).
encodecodeHist(history(yes, no,yes, no,yes, no, no, no,yes, no),277).
encodecodeHist(history( no,yes,yes, no,yes, no, no, no,yes, no),278).
encodecodeHist(history(yes,yes,yes, no,yes, no, no, no,yes, no),279).
encodecodeHist(history( no, no, no,yes,yes, no, no, no,yes, no),280).
encodecodeHist(history(yes, no, no,yes,yes, no, no, no,yes, no),281).
encodecodeHist(history( no,yes, no,yes,yes, no, no, no,yes, no),282).
encodecodeHist(history(yes,yes, no,yes,yes, no, no, no,yes, no),283).
encodecodeHist(history( no, no,yes,yes,yes, no, no, no,yes, no),284).
encodecodeHist(history(yes, no,yes,yes,yes, no, no, no,yes, no),285).
encodecodeHist(history( no,yes,yes,yes,yes, no, no, no,yes, no),286).
encodecodeHist(history(yes,yes,yes,yes,yes, no, no, no,yes, no),287).
encodecodeHist(history( no, no, no, no, no,yes, no, no,yes, no),288).
encodecodeHist(history(yes, no, no, no, no,yes, no, no,yes, no),289).
encodecodeHist(history( no,yes, no, no, no,yes, no, no,yes, no),290).
encodecodeHist(history(yes,yes, no, no, no,yes, no, no,yes, no),291).
encodecodeHist(history( no, no,yes, no, no,yes, no, no,yes, no),292).
encodecodeHist(history(yes, no,yes, no, no,yes, no, no,yes, no),293).
encodecodeHist(history( no,yes,yes, no, no,yes, no, no,yes, no),294).
encodecodeHist(history(yes,yes,yes, no, no,yes, no, no,yes, no),295).
encodecodeHist(history( no, no, no,yes, no,yes, no, no,yes, no),296).
encodecodeHist(history(yes, no, no,yes, no,yes, no, no,yes, no),297).
encodecodeHist(history( no,yes, no,yes, no,yes, no, no,yes, no),298).
encodecodeHist(history(yes,yes, no,yes, no,yes, no, no,yes, no),299).
encodecodeHist(history( no, no,yes,yes, no,yes, no, no,yes, no),300).
encodecodeHist(history(yes, no,yes,yes, no,yes, no, no,yes, no),301).
encodecodeHist(history( no,yes,yes,yes, no,yes, no, no,yes, no),302).
encodecodeHist(history(yes,yes,yes,yes, no,yes, no, no,yes, no),303).
encodecodeHist(history( no, no, no, no,yes,yes, no, no,yes, no),304).
encodecodeHist(history(yes, no, no, no,yes,yes, no, no,yes, no),305).
encodecodeHist(history( no,yes, no, no,yes,yes, no, no,yes, no),306).
encodecodeHist(history(yes,yes, no, no,yes,yes, no, no,yes, no),307).
encodecodeHist(history( no, no,yes, no,yes,yes, no, no,yes, no),308).
encodecodeHist(history(yes, no,yes, no,yes,yes, no, no,yes, no),309).
encodecodeHist(history( no,yes,yes, no,yes,yes, no, no,yes, no),310).
encodecodeHist(history(yes,yes,yes, no,yes,yes, no, no,yes, no),311).
encodecodeHist(history( no, no, no,yes,yes,yes, no, no,yes, no),312).
encodecodeHist(history(yes, no, no,yes,yes,yes, no, no,yes, no),313).
encodecodeHist(history( no,yes, no,yes,yes,yes, no, no,yes, no),314).
encodecodeHist(history(yes,yes, no,yes,yes,yes, no, no,yes, no),315).
encodecodeHist(history( no, no,yes,yes,yes,yes, no, no,yes, no),316).
encodecodeHist(history(yes, no,yes,yes,yes,yes, no, no,yes, no),317).
encodecodeHist(history( no,yes,yes,yes,yes,yes, no, no,yes, no),318).
encodecodeHist(history(yes,yes,yes,yes,yes,yes, no, no,yes, no),319).
encodecodeHist(history( no, no, no, no, no, no,yes, no,yes, no),320).
encodecodeHist(history(yes, no, no, no, no, no,yes, no,yes, no),321).
encodecodeHist(history( no,yes, no, no, no, no,yes, no,yes, no),322).
encodecodeHist(history(yes,yes, no, no, no, no,yes, no,yes, no),323).
encodecodeHist(history( no, no,yes, no, no, no,yes, no,yes, no),324).
encodecodeHist(history(yes, no,yes, no, no, no,yes, no,yes, no),325).
encodecodeHist(history( no,yes,yes, no, no, no,yes, no,yes, no),326).
encodecodeHist(history(yes,yes,yes, no, no, no,yes, no,yes, no),327).
encodecodeHist(history( no, no, no,yes, no, no,yes, no,yes, no),328).
encodecodeHist(history(yes, no, no,yes, no, no,yes, no,yes, no),329).
encodecodeHist(history( no,yes, no,yes, no, no,yes, no,yes, no),330).
encodecodeHist(history(yes,yes, no,yes, no, no,yes, no,yes, no),331).
encodecodeHist(history( no, no,yes,yes, no, no,yes, no,yes, no),332).
encodecodeHist(history(yes, no,yes,yes, no, no,yes, no,yes, no),333).
encodecodeHist(history( no,yes,yes,yes, no, no,yes, no,yes, no),334).
encodecodeHist(history(yes,yes,yes,yes, no, no,yes, no,yes, no),335).
encodecodeHist(history( no, no, no, no,yes, no,yes, no,yes, no),336).
encodecodeHist(history(yes, no, no, no,yes, no,yes, no,yes, no),337).
encodecodeHist(history( no,yes, no, no,yes, no,yes, no,yes, no),338).
encodecodeHist(history(yes,yes, no, no,yes, no,yes, no,yes, no),339).
encodecodeHist(history( no, no,yes, no,yes, no,yes, no,yes, no),340).
encodecodeHist(history(yes, no,yes, no,yes, no,yes, no,yes, no),341).
encodecodeHist(history( no,yes,yes, no,yes, no,yes, no,yes, no),342).
encodecodeHist(history(yes,yes,yes, no,yes, no,yes, no,yes, no),343).
encodecodeHist(history( no, no, no,yes,yes, no,yes, no,yes, no),344).
encodecodeHist(history(yes, no, no,yes,yes, no,yes, no,yes, no),345).
encodecodeHist(history( no,yes, no,yes,yes, no,yes, no,yes, no),346).
encodecodeHist(history(yes,yes, no,yes,yes, no,yes, no,yes, no),347).
encodecodeHist(history( no, no,yes,yes,yes, no,yes, no,yes, no),348).
encodecodeHist(history(yes, no,yes,yes,yes, no,yes, no,yes, no),349).
encodecodeHist(history( no,yes,yes,yes,yes, no,yes, no,yes, no),350).
encodecodeHist(history(yes,yes,yes,yes,yes, no,yes, no,yes, no),351).
encodecodeHist(history( no, no, no, no, no,yes,yes, no,yes, no),352).
encodecodeHist(history(yes, no, no, no, no,yes,yes, no,yes, no),353).
encodecodeHist(history( no,yes, no, no, no,yes,yes, no,yes, no),354).
encodecodeHist(history(yes,yes, no, no, no,yes,yes, no,yes, no),355).
encodecodeHist(history( no, no,yes, no, no,yes,yes, no,yes, no),356).
encodecodeHist(history(yes, no,yes, no, no,yes,yes, no,yes, no),357).
encodecodeHist(history( no,yes,yes, no, no,yes,yes, no,yes, no),358).
encodecodeHist(history(yes,yes,yes, no, no,yes,yes, no,yes, no),359).
encodecodeHist(history( no, no, no,yes, no,yes,yes, no,yes, no),360).
encodecodeHist(history(yes, no, no,yes, no,yes,yes, no,yes, no),361).
encodecodeHist(history( no,yes, no,yes, no,yes,yes, no,yes, no),362).
encodecodeHist(history(yes,yes, no,yes, no,yes,yes, no,yes, no),363).
encodecodeHist(history( no, no,yes,yes, no,yes,yes, no,yes, no),364).
encodecodeHist(history(yes, no,yes,yes, no,yes,yes, no,yes, no),365).
encodecodeHist(history( no,yes,yes,yes, no,yes,yes, no,yes, no),366).
encodecodeHist(history(yes,yes,yes,yes, no,yes,yes, no,yes, no),367).
encodecodeHist(history( no, no, no, no,yes,yes,yes, no,yes, no),368).
encodecodeHist(history(yes, no, no, no,yes,yes,yes, no,yes, no),369).
encodecodeHist(history( no,yes, no, no,yes,yes,yes, no,yes, no),370).
encodecodeHist(history(yes,yes, no, no,yes,yes,yes, no,yes, no),371).
encodecodeHist(history( no, no,yes, no,yes,yes,yes, no,yes, no),372).
encodecodeHist(history(yes, no,yes, no,yes,yes,yes, no,yes, no),373).
encodecodeHist(history( no,yes,yes, no,yes,yes,yes, no,yes, no),374).
encodecodeHist(history(yes,yes,yes, no,yes,yes,yes, no,yes, no),375).
encodecodeHist(history( no, no, no,yes,yes,yes,yes, no,yes, no),376).
encodecodeHist(history(yes, no, no,yes,yes,yes,yes, no,yes, no),377).
encodecodeHist(history( no,yes, no,yes,yes,yes,yes, no,yes, no),378).
encodecodeHist(history(yes,yes, no,yes,yes,yes,yes, no,yes, no),379).
encodecodeHist(history( no, no,yes,yes,yes,yes,yes, no,yes, no),380).
encodecodeHist(history(yes, no,yes,yes,yes,yes,yes, no,yes, no),381).
encodecodeHist(history( no,yes,yes,yes,yes,yes,yes, no,yes, no),382).
encodecodeHist(history(yes,yes,yes,yes,yes,yes,yes, no,yes, no),383).
encodecodeHist(history( no, no, no, no, no, no, no,yes,yes, no),384).
encodecodeHist(history(yes, no, no, no, no, no, no,yes,yes, no),385).
encodecodeHist(history( no,yes, no, no, no, no, no,yes,yes, no),386).
encodecodeHist(history(yes,yes, no, no, no, no, no,yes,yes, no),387).
encodecodeHist(history( no, no,yes, no, no, no, no,yes,yes, no),388).
encodecodeHist(history(yes, no,yes, no, no, no, no,yes,yes, no),389).
encodecodeHist(history( no,yes,yes, no, no, no, no,yes,yes, no),390).
encodecodeHist(history(yes,yes,yes, no, no, no, no,yes,yes, no),391).
encodecodeHist(history( no, no, no,yes, no, no, no,yes,yes, no),392).
encodecodeHist(history(yes, no, no,yes, no, no, no,yes,yes, no),393).
encodecodeHist(history( no,yes, no,yes, no, no, no,yes,yes, no),394).
encodecodeHist(history(yes,yes, no,yes, no, no, no,yes,yes, no),395).
encodecodeHist(history( no, no,yes,yes, no, no, no,yes,yes, no),396).
encodecodeHist(history(yes, no,yes,yes, no, no, no,yes,yes, no),397).
encodecodeHist(history( no,yes,yes,yes, no, no, no,yes,yes, no),398).
encodecodeHist(history(yes,yes,yes,yes, no, no, no,yes,yes, no),399).
encodecodeHist(history( no, no, no, no,yes, no, no,yes,yes, no),400).
encodecodeHist(history(yes, no, no, no,yes, no, no,yes,yes, no),401).
encodecodeHist(history( no,yes, no, no,yes, no, no,yes,yes, no),402).
encodecodeHist(history(yes,yes, no, no,yes, no, no,yes,yes, no),403).
encodecodeHist(history( no, no,yes, no,yes, no, no,yes,yes, no),404).
encodecodeHist(history(yes, no,yes, no,yes, no, no,yes,yes, no),405).
encodecodeHist(history( no,yes,yes, no,yes, no, no,yes,yes, no),406).
encodecodeHist(history(yes,yes,yes, no,yes, no, no,yes,yes, no),407).
encodecodeHist(history( no, no, no,yes,yes, no, no,yes,yes, no),408).
encodecodeHist(history(yes, no, no,yes,yes, no, no,yes,yes, no),409).
encodecodeHist(history( no,yes, no,yes,yes, no, no,yes,yes, no),410).
encodecodeHist(history(yes,yes, no,yes,yes, no, no,yes,yes, no),411).
encodecodeHist(history( no, no,yes,yes,yes, no, no,yes,yes, no),412).
encodecodeHist(history(yes, no,yes,yes,yes, no, no,yes,yes, no),413).
encodecodeHist(history( no,yes,yes,yes,yes, no, no,yes,yes, no),414).
encodecodeHist(history(yes,yes,yes,yes,yes, no, no,yes,yes, no),415).
encodecodeHist(history( no, no, no, no, no,yes, no,yes,yes, no),416).
encodecodeHist(history(yes, no, no, no, no,yes, no,yes,yes, no),417).
encodecodeHist(history( no,yes, no, no, no,yes, no,yes,yes, no),418).
encodecodeHist(history(yes,yes, no, no, no,yes, no,yes,yes, no),419).
encodecodeHist(history( no, no,yes, no, no,yes, no,yes,yes, no),420).
encodecodeHist(history(yes, no,yes, no, no,yes, no,yes,yes, no),421).
encodecodeHist(history( no,yes,yes, no, no,yes, no,yes,yes, no),422).
encodecodeHist(history(yes,yes,yes, no, no,yes, no,yes,yes, no),423).
encodecodeHist(history( no, no, no,yes, no,yes, no,yes,yes, no),424).
encodecodeHist(history(yes, no, no,yes, no,yes, no,yes,yes, no),425).
encodecodeHist(history( no,yes, no,yes, no,yes, no,yes,yes, no),426).
encodecodeHist(history(yes,yes, no,yes, no,yes, no,yes,yes, no),427).
encodecodeHist(history( no, no,yes,yes, no,yes, no,yes,yes, no),428).
encodecodeHist(history(yes, no,yes,yes, no,yes, no,yes,yes, no),429).
encodecodeHist(history( no,yes,yes,yes, no,yes, no,yes,yes, no),430).
encodecodeHist(history(yes,yes,yes,yes, no,yes, no,yes,yes, no),431).
encodecodeHist(history( no, no, no, no,yes,yes, no,yes,yes, no),432).
encodecodeHist(history(yes, no, no, no,yes,yes, no,yes,yes, no),433).
encodecodeHist(history( no,yes, no, no,yes,yes, no,yes,yes, no),434).
encodecodeHist(history(yes,yes, no, no,yes,yes, no,yes,yes, no),435).
encodecodeHist(history( no, no,yes, no,yes,yes, no,yes,yes, no),436).
encodecodeHist(history(yes, no,yes, no,yes,yes, no,yes,yes, no),437).
encodecodeHist(history( no,yes,yes, no,yes,yes, no,yes,yes, no),438).
encodecodeHist(history(yes,yes,yes, no,yes,yes, no,yes,yes, no),439).
encodecodeHist(history( no, no, no,yes,yes,yes, no,yes,yes, no),440).
encodecodeHist(history(yes, no, no,yes,yes,yes, no,yes,yes, no),441).
encodecodeHist(history( no,yes, no,yes,yes,yes, no,yes,yes, no),442).
encodecodeHist(history(yes,yes, no,yes,yes,yes, no,yes,yes, no),443).
encodecodeHist(history( no, no,yes,yes,yes,yes, no,yes,yes, no),444).
encodecodeHist(history(yes, no,yes,yes,yes,yes, no,yes,yes, no),445).
encodecodeHist(history( no,yes,yes,yes,yes,yes, no,yes,yes, no),446).
encodecodeHist(history(yes,yes,yes,yes,yes,yes, no,yes,yes, no),447).
encodecodeHist(history( no, no, no, no, no, no,yes,yes,yes, no),448).
encodecodeHist(history(yes, no, no, no, no, no,yes,yes,yes, no),449).
encodecodeHist(history( no,yes, no, no, no, no,yes,yes,yes, no),450).
encodecodeHist(history(yes,yes, no, no, no, no,yes,yes,yes, no),451).
encodecodeHist(history( no, no,yes, no, no, no,yes,yes,yes, no),452).
encodecodeHist(history(yes, no,yes, no, no, no,yes,yes,yes, no),453).
encodecodeHist(history( no,yes,yes, no, no, no,yes,yes,yes, no),454).
encodecodeHist(history(yes,yes,yes, no, no, no,yes,yes,yes, no),455).
encodecodeHist(history( no, no, no,yes, no, no,yes,yes,yes, no),456).
encodecodeHist(history(yes, no, no,yes, no, no,yes,yes,yes, no),457).
encodecodeHist(history( no,yes, no,yes, no, no,yes,yes,yes, no),458).
encodecodeHist(history(yes,yes, no,yes, no, no,yes,yes,yes, no),459).
encodecodeHist(history( no, no,yes,yes, no, no,yes,yes,yes, no),460).
encodecodeHist(history(yes, no,yes,yes, no, no,yes,yes,yes, no),461).
encodecodeHist(history( no,yes,yes,yes, no, no,yes,yes,yes, no),462).
encodecodeHist(history(yes,yes,yes,yes, no, no,yes,yes,yes, no),463).
encodecodeHist(history( no, no, no, no,yes, no,yes,yes,yes, no),464).
encodecodeHist(history(yes, no, no, no,yes, no,yes,yes,yes, no),465).
encodecodeHist(history( no,yes, no, no,yes, no,yes,yes,yes, no),466).
encodecodeHist(history(yes,yes, no, no,yes, no,yes,yes,yes, no),467).
encodecodeHist(history( no, no,yes, no,yes, no,yes,yes,yes, no),468).
encodecodeHist(history(yes, no,yes, no,yes, no,yes,yes,yes, no),469).
encodecodeHist(history( no,yes,yes, no,yes, no,yes,yes,yes, no),470).
encodecodeHist(history(yes,yes,yes, no,yes, no,yes,yes,yes, no),471).
encodecodeHist(history( no, no, no,yes,yes, no,yes,yes,yes, no),472).
encodecodeHist(history(yes, no, no,yes,yes, no,yes,yes,yes, no),473).
encodecodeHist(history( no,yes, no,yes,yes, no,yes,yes,yes, no),474).
encodecodeHist(history(yes,yes, no,yes,yes, no,yes,yes,yes, no),475).
encodecodeHist(history( no, no,yes,yes,yes, no,yes,yes,yes, no),476).
encodecodeHist(history(yes, no,yes,yes,yes, no,yes,yes,yes, no),477).
encodecodeHist(history( no,yes,yes,yes,yes, no,yes,yes,yes, no),478).
encodecodeHist(history(yes,yes,yes,yes,yes, no,yes,yes,yes, no),479).
encodecodeHist(history( no, no, no, no, no,yes,yes,yes,yes, no),480).
encodecodeHist(history(yes, no, no, no, no,yes,yes,yes,yes, no),481).
encodecodeHist(history( no,yes, no, no, no,yes,yes,yes,yes, no),482).
encodecodeHist(history(yes,yes, no, no, no,yes,yes,yes,yes, no),483).
encodecodeHist(history( no, no,yes, no, no,yes,yes,yes,yes, no),484).
encodecodeHist(history(yes, no,yes, no, no,yes,yes,yes,yes, no),485).
encodecodeHist(history( no,yes,yes, no, no,yes,yes,yes,yes, no),486).
encodecodeHist(history(yes,yes,yes, no, no,yes,yes,yes,yes, no),487).
encodecodeHist(history( no, no, no,yes, no,yes,yes,yes,yes, no),488).
encodecodeHist(history(yes, no, no,yes, no,yes,yes,yes,yes, no),489).
encodecodeHist(history( no,yes, no,yes, no,yes,yes,yes,yes, no),490).
encodecodeHist(history(yes,yes, no,yes, no,yes,yes,yes,yes, no),491).
encodecodeHist(history( no, no,yes,yes, no,yes,yes,yes,yes, no),492).
encodecodeHist(history(yes, no,yes,yes, no,yes,yes,yes,yes, no),493).
encodecodeHist(history( no,yes,yes,yes, no,yes,yes,yes,yes, no),494).
encodecodeHist(history(yes,yes,yes,yes, no,yes,yes,yes,yes, no),495).
encodecodeHist(history( no, no, no, no,yes,yes,yes,yes,yes, no),496).
encodecodeHist(history(yes, no, no, no,yes,yes,yes,yes,yes, no),497).
encodecodeHist(history( no,yes, no, no,yes,yes,yes,yes,yes, no),498).
encodecodeHist(history(yes,yes, no, no,yes,yes,yes,yes,yes, no),499).
encodecodeHist(history( no, no,yes, no,yes,yes,yes,yes,yes, no),500).
encodecodeHist(history(yes, no,yes, no,yes,yes,yes,yes,yes, no),501).
encodecodeHist(history( no,yes,yes, no,yes,yes,yes,yes,yes, no),502).
encodecodeHist(history(yes,yes,yes, no,yes,yes,yes,yes,yes, no),503).
encodecodeHist(history( no, no, no,yes,yes,yes,yes,yes,yes, no),504).
encodecodeHist(history(yes, no, no,yes,yes,yes,yes,yes,yes, no),505).
encodecodeHist(history( no,yes, no,yes,yes,yes,yes,yes,yes, no),506).
encodecodeHist(history(yes,yes, no,yes,yes,yes,yes,yes,yes, no),507).
encodecodeHist(history( no, no,yes,yes,yes,yes,yes,yes,yes, no),508).
encodecodeHist(history(yes, no,yes,yes,yes,yes,yes,yes,yes, no),509).
encodecodeHist(history( no,yes,yes,yes,yes,yes,yes,yes,yes, no),510).
encodecodeHist(history(yes,yes,yes,yes,yes,yes,yes,yes,yes, no),511).
encodecodeHist(history( no, no, no, no, no, no, no, no, no,yes),512).
encodecodeHist(history(yes, no, no, no, no, no, no, no, no,yes),513).
encodecodeHist(history( no,yes, no, no, no, no, no, no, no,yes),514).
encodecodeHist(history(yes,yes, no, no, no, no, no, no, no,yes),515).
encodecodeHist(history( no, no,yes, no, no, no, no, no, no,yes),516).
encodecodeHist(history(yes, no,yes, no, no, no, no, no, no,yes),517).
encodecodeHist(history( no,yes,yes, no, no, no, no, no, no,yes),518).
encodecodeHist(history(yes,yes,yes, no, no, no, no, no, no,yes),519).
encodecodeHist(history( no, no, no,yes, no, no, no, no, no,yes),520).
encodecodeHist(history(yes, no, no,yes, no, no, no, no, no,yes),521).
encodecodeHist(history( no,yes, no,yes, no, no, no, no, no,yes),522).
encodecodeHist(history(yes,yes, no,yes, no, no, no, no, no,yes),523).
encodecodeHist(history( no, no,yes,yes, no, no, no, no, no,yes),524).
encodecodeHist(history(yes, no,yes,yes, no, no, no, no, no,yes),525).
encodecodeHist(history( no,yes,yes,yes, no, no, no, no, no,yes),526).
encodecodeHist(history(yes,yes,yes,yes, no, no, no, no, no,yes),527).
encodecodeHist(history( no, no, no, no,yes, no, no, no, no,yes),528).
encodecodeHist(history(yes, no, no, no,yes, no, no, no, no,yes),529).
encodecodeHist(history( no,yes, no, no,yes, no, no, no, no,yes),530).
encodecodeHist(history(yes,yes, no, no,yes, no, no, no, no,yes),531).
encodecodeHist(history( no, no,yes, no,yes, no, no, no, no,yes),532).
encodecodeHist(history(yes, no,yes, no,yes, no, no, no, no,yes),533).
encodecodeHist(history( no,yes,yes, no,yes, no, no, no, no,yes),534).
encodecodeHist(history(yes,yes,yes, no,yes, no, no, no, no,yes),535).
encodecodeHist(history( no, no, no,yes,yes, no, no, no, no,yes),536).
encodecodeHist(history(yes, no, no,yes,yes, no, no, no, no,yes),537).
encodecodeHist(history( no,yes, no,yes,yes, no, no, no, no,yes),538).
encodecodeHist(history(yes,yes, no,yes,yes, no, no, no, no,yes),539).
encodecodeHist(history( no, no,yes,yes,yes, no, no, no, no,yes),540).
encodecodeHist(history(yes, no,yes,yes,yes, no, no, no, no,yes),541).
encodecodeHist(history( no,yes,yes,yes,yes, no, no, no, no,yes),542).
encodecodeHist(history(yes,yes,yes,yes,yes, no, no, no, no,yes),543).
encodecodeHist(history( no, no, no, no, no,yes, no, no, no,yes),544).
encodecodeHist(history(yes, no, no, no, no,yes, no, no, no,yes),545).
encodecodeHist(history( no,yes, no, no, no,yes, no, no, no,yes),546).
encodecodeHist(history(yes,yes, no, no, no,yes, no, no, no,yes),547).
encodecodeHist(history( no, no,yes, no, no,yes, no, no, no,yes),548).
encodecodeHist(history(yes, no,yes, no, no,yes, no, no, no,yes),549).
encodecodeHist(history( no,yes,yes, no, no,yes, no, no, no,yes),550).
encodecodeHist(history(yes,yes,yes, no, no,yes, no, no, no,yes),551).
encodecodeHist(history( no, no, no,yes, no,yes, no, no, no,yes),552).
encodecodeHist(history(yes, no, no,yes, no,yes, no, no, no,yes),553).
encodecodeHist(history( no,yes, no,yes, no,yes, no, no, no,yes),554).
encodecodeHist(history(yes,yes, no,yes, no,yes, no, no, no,yes),555).
encodecodeHist(history( no, no,yes,yes, no,yes, no, no, no,yes),556).
encodecodeHist(history(yes, no,yes,yes, no,yes, no, no, no,yes),557).
encodecodeHist(history( no,yes,yes,yes, no,yes, no, no, no,yes),558).
encodecodeHist(history(yes,yes,yes,yes, no,yes, no, no, no,yes),559).
encodecodeHist(history( no, no, no, no,yes,yes, no, no, no,yes),560).
encodecodeHist(history(yes, no, no, no,yes,yes, no, no, no,yes),561).
encodecodeHist(history( no,yes, no, no,yes,yes, no, no, no,yes),562).
encodecodeHist(history(yes,yes, no, no,yes,yes, no, no, no,yes),563).
encodecodeHist(history( no, no,yes, no,yes,yes, no, no, no,yes),564).
encodecodeHist(history(yes, no,yes, no,yes,yes, no, no, no,yes),565).
encodecodeHist(history( no,yes,yes, no,yes,yes, no, no, no,yes),566).
encodecodeHist(history(yes,yes,yes, no,yes,yes, no, no, no,yes),567).
encodecodeHist(history( no, no, no,yes,yes,yes, no, no, no,yes),568).
encodecodeHist(history(yes, no, no,yes,yes,yes, no, no, no,yes),569).
encodecodeHist(history( no,yes, no,yes,yes,yes, no, no, no,yes),570).
encodecodeHist(history(yes,yes, no,yes,yes,yes, no, no, no,yes),571).
encodecodeHist(history( no, no,yes,yes,yes,yes, no, no, no,yes),572).
encodecodeHist(history(yes, no,yes,yes,yes,yes, no, no, no,yes),573).
encodecodeHist(history( no,yes,yes,yes,yes,yes, no, no, no,yes),574).
encodecodeHist(history(yes,yes,yes,yes,yes,yes, no, no, no,yes),575).
encodecodeHist(history( no, no, no, no, no, no,yes, no, no,yes),576).
encodecodeHist(history(yes, no, no, no, no, no,yes, no, no,yes),577).
encodecodeHist(history( no,yes, no, no, no, no,yes, no, no,yes),578).
encodecodeHist(history(yes,yes, no, no, no, no,yes, no, no,yes),579).
encodecodeHist(history( no, no,yes, no, no, no,yes, no, no,yes),580).
encodecodeHist(history(yes, no,yes, no, no, no,yes, no, no,yes),581).
encodecodeHist(history( no,yes,yes, no, no, no,yes, no, no,yes),582).
encodecodeHist(history(yes,yes,yes, no, no, no,yes, no, no,yes),583).
encodecodeHist(history( no, no, no,yes, no, no,yes, no, no,yes),584).
encodecodeHist(history(yes, no, no,yes, no, no,yes, no, no,yes),585).
encodecodeHist(history( no,yes, no,yes, no, no,yes, no, no,yes),586).
encodecodeHist(history(yes,yes, no,yes, no, no,yes, no, no,yes),587).
encodecodeHist(history( no, no,yes,yes, no, no,yes, no, no,yes),588).
encodecodeHist(history(yes, no,yes,yes, no, no,yes, no, no,yes),589).
encodecodeHist(history( no,yes,yes,yes, no, no,yes, no, no,yes),590).
encodecodeHist(history(yes,yes,yes,yes, no, no,yes, no, no,yes),591).
encodecodeHist(history( no, no, no, no,yes, no,yes, no, no,yes),592).
encodecodeHist(history(yes, no, no, no,yes, no,yes, no, no,yes),593).
encodecodeHist(history( no,yes, no, no,yes, no,yes, no, no,yes),594).
encodecodeHist(history(yes,yes, no, no,yes, no,yes, no, no,yes),595).
encodecodeHist(history( no, no,yes, no,yes, no,yes, no, no,yes),596).
encodecodeHist(history(yes, no,yes, no,yes, no,yes, no, no,yes),597).
encodecodeHist(history( no,yes,yes, no,yes, no,yes, no, no,yes),598).
encodecodeHist(history(yes,yes,yes, no,yes, no,yes, no, no,yes),599).
encodecodeHist(history( no, no, no,yes,yes, no,yes, no, no,yes),600).
encodecodeHist(history(yes, no, no,yes,yes, no,yes, no, no,yes),601).
encodecodeHist(history( no,yes, no,yes,yes, no,yes, no, no,yes),602).
encodecodeHist(history(yes,yes, no,yes,yes, no,yes, no, no,yes),603).
encodecodeHist(history( no, no,yes,yes,yes, no,yes, no, no,yes),604).
encodecodeHist(history(yes, no,yes,yes,yes, no,yes, no, no,yes),605).
encodecodeHist(history( no,yes,yes,yes,yes, no,yes, no, no,yes),606).
encodecodeHist(history(yes,yes,yes,yes,yes, no,yes, no, no,yes),607).
encodecodeHist(history( no, no, no, no, no,yes,yes, no, no,yes),608).
encodecodeHist(history(yes, no, no, no, no,yes,yes, no, no,yes),609).
encodecodeHist(history( no,yes, no, no, no,yes,yes, no, no,yes),610).
encodecodeHist(history(yes,yes, no, no, no,yes,yes, no, no,yes),611).
encodecodeHist(history( no, no,yes, no, no,yes,yes, no, no,yes),612).
encodecodeHist(history(yes, no,yes, no, no,yes,yes, no, no,yes),613).
encodecodeHist(history( no,yes,yes, no, no,yes,yes, no, no,yes),614).
encodecodeHist(history(yes,yes,yes, no, no,yes,yes, no, no,yes),615).
encodecodeHist(history( no, no, no,yes, no,yes,yes, no, no,yes),616).
encodecodeHist(history(yes, no, no,yes, no,yes,yes, no, no,yes),617).
encodecodeHist(history( no,yes, no,yes, no,yes,yes, no, no,yes),618).
encodecodeHist(history(yes,yes, no,yes, no,yes,yes, no, no,yes),619).
encodecodeHist(history( no, no,yes,yes, no,yes,yes, no, no,yes),620).
encodecodeHist(history(yes, no,yes,yes, no,yes,yes, no, no,yes),621).
encodecodeHist(history( no,yes,yes,yes, no,yes,yes, no, no,yes),622).
encodecodeHist(history(yes,yes,yes,yes, no,yes,yes, no, no,yes),623).
encodecodeHist(history( no, no, no, no,yes,yes,yes, no, no,yes),624).
encodecodeHist(history(yes, no, no, no,yes,yes,yes, no, no,yes),625).
encodecodeHist(history( no,yes, no, no,yes,yes,yes, no, no,yes),626).
encodecodeHist(history(yes,yes, no, no,yes,yes,yes, no, no,yes),627).
encodecodeHist(history( no, no,yes, no,yes,yes,yes, no, no,yes),628).
encodecodeHist(history(yes, no,yes, no,yes,yes,yes, no, no,yes),629).
encodecodeHist(history( no,yes,yes, no,yes,yes,yes, no, no,yes),630).
encodecodeHist(history(yes,yes,yes, no,yes,yes,yes, no, no,yes),631).
encodecodeHist(history( no, no, no,yes,yes,yes,yes, no, no,yes),632).
encodecodeHist(history(yes, no, no,yes,yes,yes,yes, no, no,yes),633).
encodecodeHist(history( no,yes, no,yes,yes,yes,yes, no, no,yes),634).
encodecodeHist(history(yes,yes, no,yes,yes,yes,yes, no, no,yes),635).
encodecodeHist(history( no, no,yes,yes,yes,yes,yes, no, no,yes),636).
encodecodeHist(history(yes, no,yes,yes,yes,yes,yes, no, no,yes),637).
encodecodeHist(history( no,yes,yes,yes,yes,yes,yes, no, no,yes),638).
encodecodeHist(history(yes,yes,yes,yes,yes,yes,yes, no, no,yes),639).
encodecodeHist(history( no, no, no, no, no, no, no,yes, no,yes),640).
encodecodeHist(history(yes, no, no, no, no, no, no,yes, no,yes),641).
encodecodeHist(history( no,yes, no, no, no, no, no,yes, no,yes),642).
encodecodeHist(history(yes,yes, no, no, no, no, no,yes, no,yes),643).
encodecodeHist(history( no, no,yes, no, no, no, no,yes, no,yes),644).
encodecodeHist(history(yes, no,yes, no, no, no, no,yes, no,yes),645).
encodecodeHist(history( no,yes,yes, no, no, no, no,yes, no,yes),646).
encodecodeHist(history(yes,yes,yes, no, no, no, no,yes, no,yes),647).
encodecodeHist(history( no, no, no,yes, no, no, no,yes, no,yes),648).
encodecodeHist(history(yes, no, no,yes, no, no, no,yes, no,yes),649).
encodecodeHist(history( no,yes, no,yes, no, no, no,yes, no,yes),650).
encodecodeHist(history(yes,yes, no,yes, no, no, no,yes, no,yes),651).
encodecodeHist(history( no, no,yes,yes, no, no, no,yes, no,yes),652).
encodecodeHist(history(yes, no,yes,yes, no, no, no,yes, no,yes),653).
encodecodeHist(history( no,yes,yes,yes, no, no, no,yes, no,yes),654).
encodecodeHist(history(yes,yes,yes,yes, no, no, no,yes, no,yes),655).
encodecodeHist(history( no, no, no, no,yes, no, no,yes, no,yes),656).
encodecodeHist(history(yes, no, no, no,yes, no, no,yes, no,yes),657).
encodecodeHist(history( no,yes, no, no,yes, no, no,yes, no,yes),658).
encodecodeHist(history(yes,yes, no, no,yes, no, no,yes, no,yes),659).
encodecodeHist(history( no, no,yes, no,yes, no, no,yes, no,yes),660).
encodecodeHist(history(yes, no,yes, no,yes, no, no,yes, no,yes),661).
encodecodeHist(history( no,yes,yes, no,yes, no, no,yes, no,yes),662).
encodecodeHist(history(yes,yes,yes, no,yes, no, no,yes, no,yes),663).
encodecodeHist(history( no, no, no,yes,yes, no, no,yes, no,yes),664).
encodecodeHist(history(yes, no, no,yes,yes, no, no,yes, no,yes),665).
encodecodeHist(history( no,yes, no,yes,yes, no, no,yes, no,yes),666).
encodecodeHist(history(yes,yes, no,yes,yes, no, no,yes, no,yes),667).
encodecodeHist(history( no, no,yes,yes,yes, no, no,yes, no,yes),668).
encodecodeHist(history(yes, no,yes,yes,yes, no, no,yes, no,yes),669).
encodecodeHist(history( no,yes,yes,yes,yes, no, no,yes, no,yes),670).
encodecodeHist(history(yes,yes,yes,yes,yes, no, no,yes, no,yes),671).
encodecodeHist(history( no, no, no, no, no,yes, no,yes, no,yes),672).
encodecodeHist(history(yes, no, no, no, no,yes, no,yes, no,yes),673).
encodecodeHist(history( no,yes, no, no, no,yes, no,yes, no,yes),674).
encodecodeHist(history(yes,yes, no, no, no,yes, no,yes, no,yes),675).
encodecodeHist(history( no, no,yes, no, no,yes, no,yes, no,yes),676).
encodecodeHist(history(yes, no,yes, no, no,yes, no,yes, no,yes),677).
encodecodeHist(history( no,yes,yes, no, no,yes, no,yes, no,yes),678).
encodecodeHist(history(yes,yes,yes, no, no,yes, no,yes, no,yes),679).
encodecodeHist(history( no, no, no,yes, no,yes, no,yes, no,yes),680).
encodecodeHist(history(yes, no, no,yes, no,yes, no,yes, no,yes),681).
encodecodeHist(history( no,yes, no,yes, no,yes, no,yes, no,yes),682).
encodecodeHist(history(yes,yes, no,yes, no,yes, no,yes, no,yes),683).
encodecodeHist(history( no, no,yes,yes, no,yes, no,yes, no,yes),684).
encodecodeHist(history(yes, no,yes,yes, no,yes, no,yes, no,yes),685).
encodecodeHist(history( no,yes,yes,yes, no,yes, no,yes, no,yes),686).
encodecodeHist(history(yes,yes,yes,yes, no,yes, no,yes, no,yes),687).
encodecodeHist(history( no, no, no, no,yes,yes, no,yes, no,yes),688).
encodecodeHist(history(yes, no, no, no,yes,yes, no,yes, no,yes),689).
encodecodeHist(history( no,yes, no, no,yes,yes, no,yes, no,yes),690).
encodecodeHist(history(yes,yes, no, no,yes,yes, no,yes, no,yes),691).
encodecodeHist(history( no, no,yes, no,yes,yes, no,yes, no,yes),692).
encodecodeHist(history(yes, no,yes, no,yes,yes, no,yes, no,yes),693).
encodecodeHist(history( no,yes,yes, no,yes,yes, no,yes, no,yes),694).
encodecodeHist(history(yes,yes,yes, no,yes,yes, no,yes, no,yes),695).
encodecodeHist(history( no, no, no,yes,yes,yes, no,yes, no,yes),696).
encodecodeHist(history(yes, no, no,yes,yes,yes, no,yes, no,yes),697).
encodecodeHist(history( no,yes, no,yes,yes,yes, no,yes, no,yes),698).
encodecodeHist(history(yes,yes, no,yes,yes,yes, no,yes, no,yes),699).
encodecodeHist(history( no, no,yes,yes,yes,yes, no,yes, no,yes),700).
encodecodeHist(history(yes, no,yes,yes,yes,yes, no,yes, no,yes),701).
encodecodeHist(history( no,yes,yes,yes,yes,yes, no,yes, no,yes),702).
encodecodeHist(history(yes,yes,yes,yes,yes,yes, no,yes, no,yes),703).
encodecodeHist(history( no, no, no, no, no, no,yes,yes, no,yes),704).
encodecodeHist(history(yes, no, no, no, no, no,yes,yes, no,yes),705).
encodecodeHist(history( no,yes, no, no, no, no,yes,yes, no,yes),706).
encodecodeHist(history(yes,yes, no, no, no, no,yes,yes, no,yes),707).
encodecodeHist(history( no, no,yes, no, no, no,yes,yes, no,yes),708).
encodecodeHist(history(yes, no,yes, no, no, no,yes,yes, no,yes),709).
encodecodeHist(history( no,yes,yes, no, no, no,yes,yes, no,yes),710).
encodecodeHist(history(yes,yes,yes, no, no, no,yes,yes, no,yes),711).
encodecodeHist(history( no, no, no,yes, no, no,yes,yes, no,yes),712).
encodecodeHist(history(yes, no, no,yes, no, no,yes,yes, no,yes),713).
encodecodeHist(history( no,yes, no,yes, no, no,yes,yes, no,yes),714).
encodecodeHist(history(yes,yes, no,yes, no, no,yes,yes, no,yes),715).
encodecodeHist(history( no, no,yes,yes, no, no,yes,yes, no,yes),716).
encodecodeHist(history(yes, no,yes,yes, no, no,yes,yes, no,yes),717).
encodecodeHist(history( no,yes,yes,yes, no, no,yes,yes, no,yes),718).
encodecodeHist(history(yes,yes,yes,yes, no, no,yes,yes, no,yes),719).
encodecodeHist(history( no, no, no, no,yes, no,yes,yes, no,yes),720).
encodecodeHist(history(yes, no, no, no,yes, no,yes,yes, no,yes),721).
encodecodeHist(history( no,yes, no, no,yes, no,yes,yes, no,yes),722).
encodecodeHist(history(yes,yes, no, no,yes, no,yes,yes, no,yes),723).
encodecodeHist(history( no, no,yes, no,yes, no,yes,yes, no,yes),724).
encodecodeHist(history(yes, no,yes, no,yes, no,yes,yes, no,yes),725).
encodecodeHist(history( no,yes,yes, no,yes, no,yes,yes, no,yes),726).
encodecodeHist(history(yes,yes,yes, no,yes, no,yes,yes, no,yes),727).
encodecodeHist(history( no, no, no,yes,yes, no,yes,yes, no,yes),728).
encodecodeHist(history(yes, no, no,yes,yes, no,yes,yes, no,yes),729).
encodecodeHist(history( no,yes, no,yes,yes, no,yes,yes, no,yes),730).
encodecodeHist(history(yes,yes, no,yes,yes, no,yes,yes, no,yes),731).
encodecodeHist(history( no, no,yes,yes,yes, no,yes,yes, no,yes),732).
encodecodeHist(history(yes, no,yes,yes,yes, no,yes,yes, no,yes),733).
encodecodeHist(history( no,yes,yes,yes,yes, no,yes,yes, no,yes),734).
encodecodeHist(history(yes,yes,yes,yes,yes, no,yes,yes, no,yes),735).
encodecodeHist(history( no, no, no, no, no,yes,yes,yes, no,yes),736).
encodecodeHist(history(yes, no, no, no, no,yes,yes,yes, no,yes),737).
encodecodeHist(history( no,yes, no, no, no,yes,yes,yes, no,yes),738).
encodecodeHist(history(yes,yes, no, no, no,yes,yes,yes, no,yes),739).
encodecodeHist(history( no, no,yes, no, no,yes,yes,yes, no,yes),740).
encodecodeHist(history(yes, no,yes, no, no,yes,yes,yes, no,yes),741).
encodecodeHist(history( no,yes,yes, no, no,yes,yes,yes, no,yes),742).
encodecodeHist(history(yes,yes,yes, no, no,yes,yes,yes, no,yes),743).
encodecodeHist(history( no, no, no,yes, no,yes,yes,yes, no,yes),744).
encodecodeHist(history(yes, no, no,yes, no,yes,yes,yes, no,yes),745).
encodecodeHist(history( no,yes, no,yes, no,yes,yes,yes, no,yes),746).
encodecodeHist(history(yes,yes, no,yes, no,yes,yes,yes, no,yes),747).
encodecodeHist(history( no, no,yes,yes, no,yes,yes,yes, no,yes),748).
encodecodeHist(history(yes, no,yes,yes, no,yes,yes,yes, no,yes),749).
encodecodeHist(history( no,yes,yes,yes, no,yes,yes,yes, no,yes),750).
encodecodeHist(history(yes,yes,yes,yes, no,yes,yes,yes, no,yes),751).
encodecodeHist(history( no, no, no, no,yes,yes,yes,yes, no,yes),752).
encodecodeHist(history(yes, no, no, no,yes,yes,yes,yes, no,yes),753).
encodecodeHist(history( no,yes, no, no,yes,yes,yes,yes, no,yes),754).
encodecodeHist(history(yes,yes, no, no,yes,yes,yes,yes, no,yes),755).
encodecodeHist(history( no, no,yes, no,yes,yes,yes,yes, no,yes),756).
encodecodeHist(history(yes, no,yes, no,yes,yes,yes,yes, no,yes),757).
encodecodeHist(history( no,yes,yes, no,yes,yes,yes,yes, no,yes),758).
encodecodeHist(history(yes,yes,yes, no,yes,yes,yes,yes, no,yes),759).
encodecodeHist(history( no, no, no,yes,yes,yes,yes,yes, no,yes),760).
encodecodeHist(history(yes, no, no,yes,yes,yes,yes,yes, no,yes),761).
encodecodeHist(history( no,yes, no,yes,yes,yes,yes,yes, no,yes),762).
encodecodeHist(history(yes,yes, no,yes,yes,yes,yes,yes, no,yes),763).
encodecodeHist(history( no, no,yes,yes,yes,yes,yes,yes, no,yes),764).
encodecodeHist(history(yes, no,yes,yes,yes,yes,yes,yes, no,yes),765).
encodecodeHist(history( no,yes,yes,yes,yes,yes,yes,yes, no,yes),766).
encodecodeHist(history(yes,yes,yes,yes,yes,yes,yes,yes, no,yes),767).
encodecodeHist(history( no, no, no, no, no, no, no, no,yes,yes),768).
encodecodeHist(history(yes, no, no, no, no, no, no, no,yes,yes),769).
encodecodeHist(history( no,yes, no, no, no, no, no, no,yes,yes),770).
encodecodeHist(history(yes,yes, no, no, no, no, no, no,yes,yes),771).
encodecodeHist(history( no, no,yes, no, no, no, no, no,yes,yes),772).
encodecodeHist(history(yes, no,yes, no, no, no, no, no,yes,yes),773).
encodecodeHist(history( no,yes,yes, no, no, no, no, no,yes,yes),774).
encodecodeHist(history(yes,yes,yes, no, no, no, no, no,yes,yes),775).
encodecodeHist(history( no, no, no,yes, no, no, no, no,yes,yes),776).
encodecodeHist(history(yes, no, no,yes, no, no, no, no,yes,yes),777).
encodecodeHist(history( no,yes, no,yes, no, no, no, no,yes,yes),778).
encodecodeHist(history(yes,yes, no,yes, no, no, no, no,yes,yes),779).
encodecodeHist(history( no, no,yes,yes, no, no, no, no,yes,yes),780).
encodecodeHist(history(yes, no,yes,yes, no, no, no, no,yes,yes),781).
encodecodeHist(history( no,yes,yes,yes, no, no, no, no,yes,yes),782).
encodecodeHist(history(yes,yes,yes,yes, no, no, no, no,yes,yes),783).
encodecodeHist(history( no, no, no, no,yes, no, no, no,yes,yes),784).
encodecodeHist(history(yes, no, no, no,yes, no, no, no,yes,yes),785).
encodecodeHist(history( no,yes, no, no,yes, no, no, no,yes,yes),786).
encodecodeHist(history(yes,yes, no, no,yes, no, no, no,yes,yes),787).
encodecodeHist(history( no, no,yes, no,yes, no, no, no,yes,yes),788).
encodecodeHist(history(yes, no,yes, no,yes, no, no, no,yes,yes),789).
encodecodeHist(history( no,yes,yes, no,yes, no, no, no,yes,yes),790).
encodecodeHist(history(yes,yes,yes, no,yes, no, no, no,yes,yes),791).
encodecodeHist(history( no, no, no,yes,yes, no, no, no,yes,yes),792).
encodecodeHist(history(yes, no, no,yes,yes, no, no, no,yes,yes),793).
encodecodeHist(history( no,yes, no,yes,yes, no, no, no,yes,yes),794).
encodecodeHist(history(yes,yes, no,yes,yes, no, no, no,yes,yes),795).
encodecodeHist(history( no, no,yes,yes,yes, no, no, no,yes,yes),796).
encodecodeHist(history(yes, no,yes,yes,yes, no, no, no,yes,yes),797).
encodecodeHist(history( no,yes,yes,yes,yes, no, no, no,yes,yes),798).
encodecodeHist(history(yes,yes,yes,yes,yes, no, no, no,yes,yes),799).
encodecodeHist(history( no, no, no, no, no,yes, no, no,yes,yes),800).
encodecodeHist(history(yes, no, no, no, no,yes, no, no,yes,yes),801).
encodecodeHist(history( no,yes, no, no, no,yes, no, no,yes,yes),802).
encodecodeHist(history(yes,yes, no, no, no,yes, no, no,yes,yes),803).
encodecodeHist(history( no, no,yes, no, no,yes, no, no,yes,yes),804).
encodecodeHist(history(yes, no,yes, no, no,yes, no, no,yes,yes),805).
encodecodeHist(history( no,yes,yes, no, no,yes, no, no,yes,yes),806).
encodecodeHist(history(yes,yes,yes, no, no,yes, no, no,yes,yes),807).
encodecodeHist(history( no, no, no,yes, no,yes, no, no,yes,yes),808).
encodecodeHist(history(yes, no, no,yes, no,yes, no, no,yes,yes),809).
encodecodeHist(history( no,yes, no,yes, no,yes, no, no,yes,yes),810).
encodecodeHist(history(yes,yes, no,yes, no,yes, no, no,yes,yes),811).
encodecodeHist(history( no, no,yes,yes, no,yes, no, no,yes,yes),812).
encodecodeHist(history(yes, no,yes,yes, no,yes, no, no,yes,yes),813).
encodecodeHist(history( no,yes,yes,yes, no,yes, no, no,yes,yes),814).
encodecodeHist(history(yes,yes,yes,yes, no,yes, no, no,yes,yes),815).
encodecodeHist(history( no, no, no, no,yes,yes, no, no,yes,yes),816).
encodecodeHist(history(yes, no, no, no,yes,yes, no, no,yes,yes),817).
encodecodeHist(history( no,yes, no, no,yes,yes, no, no,yes,yes),818).
encodecodeHist(history(yes,yes, no, no,yes,yes, no, no,yes,yes),819).
encodecodeHist(history( no, no,yes, no,yes,yes, no, no,yes,yes),820).
encodecodeHist(history(yes, no,yes, no,yes,yes, no, no,yes,yes),821).
encodecodeHist(history( no,yes,yes, no,yes,yes, no, no,yes,yes),822).
encodecodeHist(history(yes,yes,yes, no,yes,yes, no, no,yes,yes),823).
encodecodeHist(history( no, no, no,yes,yes,yes, no, no,yes,yes),824).
encodecodeHist(history(yes, no, no,yes,yes,yes, no, no,yes,yes),825).
encodecodeHist(history( no,yes, no,yes,yes,yes, no, no,yes,yes),826).
encodecodeHist(history(yes,yes, no,yes,yes,yes, no, no,yes,yes),827).
encodecodeHist(history( no, no,yes,yes,yes,yes, no, no,yes,yes),828).
encodecodeHist(history(yes, no,yes,yes,yes,yes, no, no,yes,yes),829).
encodecodeHist(history( no,yes,yes,yes,yes,yes, no, no,yes,yes),830).
encodecodeHist(history(yes,yes,yes,yes,yes,yes, no, no,yes,yes),831).
encodecodeHist(history( no, no, no, no, no, no,yes, no,yes,yes),832).
encodecodeHist(history(yes, no, no, no, no, no,yes, no,yes,yes),833).
encodecodeHist(history( no,yes, no, no, no, no,yes, no,yes,yes),834).
encodecodeHist(history(yes,yes, no, no, no, no,yes, no,yes,yes),835).
encodecodeHist(history( no, no,yes, no, no, no,yes, no,yes,yes),836).
encodecodeHist(history(yes, no,yes, no, no, no,yes, no,yes,yes),837).
encodecodeHist(history( no,yes,yes, no, no, no,yes, no,yes,yes),838).
encodecodeHist(history(yes,yes,yes, no, no, no,yes, no,yes,yes),839).
encodecodeHist(history( no, no, no,yes, no, no,yes, no,yes,yes),840).
encodecodeHist(history(yes, no, no,yes, no, no,yes, no,yes,yes),841).
encodecodeHist(history( no,yes, no,yes, no, no,yes, no,yes,yes),842).
encodecodeHist(history(yes,yes, no,yes, no, no,yes, no,yes,yes),843).
encodecodeHist(history( no, no,yes,yes, no, no,yes, no,yes,yes),844).
encodecodeHist(history(yes, no,yes,yes, no, no,yes, no,yes,yes),845).
encodecodeHist(history( no,yes,yes,yes, no, no,yes, no,yes,yes),846).
encodecodeHist(history(yes,yes,yes,yes, no, no,yes, no,yes,yes),847).
encodecodeHist(history( no, no, no, no,yes, no,yes, no,yes,yes),848).
encodecodeHist(history(yes, no, no, no,yes, no,yes, no,yes,yes),849).
encodecodeHist(history( no,yes, no, no,yes, no,yes, no,yes,yes),850).
encodecodeHist(history(yes,yes, no, no,yes, no,yes, no,yes,yes),851).
encodecodeHist(history( no, no,yes, no,yes, no,yes, no,yes,yes),852).
encodecodeHist(history(yes, no,yes, no,yes, no,yes, no,yes,yes),853).
encodecodeHist(history( no,yes,yes, no,yes, no,yes, no,yes,yes),854).
encodecodeHist(history(yes,yes,yes, no,yes, no,yes, no,yes,yes),855).
encodecodeHist(history( no, no, no,yes,yes, no,yes, no,yes,yes),856).
encodecodeHist(history(yes, no, no,yes,yes, no,yes, no,yes,yes),857).
encodecodeHist(history( no,yes, no,yes,yes, no,yes, no,yes,yes),858).
encodecodeHist(history(yes,yes, no,yes,yes, no,yes, no,yes,yes),859).
encodecodeHist(history( no, no,yes,yes,yes, no,yes, no,yes,yes),860).
encodecodeHist(history(yes, no,yes,yes,yes, no,yes, no,yes,yes),861).
encodecodeHist(history( no,yes,yes,yes,yes, no,yes, no,yes,yes),862).
encodecodeHist(history(yes,yes,yes,yes,yes, no,yes, no,yes,yes),863).
encodecodeHist(history( no, no, no, no, no,yes,yes, no,yes,yes),864).
encodecodeHist(history(yes, no, no, no, no,yes,yes, no,yes,yes),865).
encodecodeHist(history( no,yes, no, no, no,yes,yes, no,yes,yes),866).
encodecodeHist(history(yes,yes, no, no, no,yes,yes, no,yes,yes),867).
encodecodeHist(history( no, no,yes, no, no,yes,yes, no,yes,yes),868).
encodecodeHist(history(yes, no,yes, no, no,yes,yes, no,yes,yes),869).
encodecodeHist(history( no,yes,yes, no, no,yes,yes, no,yes,yes),870).
encodecodeHist(history(yes,yes,yes, no, no,yes,yes, no,yes,yes),871).
encodecodeHist(history( no, no, no,yes, no,yes,yes, no,yes,yes),872).
encodecodeHist(history(yes, no, no,yes, no,yes,yes, no,yes,yes),873).
encodecodeHist(history( no,yes, no,yes, no,yes,yes, no,yes,yes),874).
encodecodeHist(history(yes,yes, no,yes, no,yes,yes, no,yes,yes),875).
encodecodeHist(history( no, no,yes,yes, no,yes,yes, no,yes,yes),876).
encodecodeHist(history(yes, no,yes,yes, no,yes,yes, no,yes,yes),877).
encodecodeHist(history( no,yes,yes,yes, no,yes,yes, no,yes,yes),878).
encodecodeHist(history(yes,yes,yes,yes, no,yes,yes, no,yes,yes),879).
encodecodeHist(history( no, no, no, no,yes,yes,yes, no,yes,yes),880).
encodecodeHist(history(yes, no, no, no,yes,yes,yes, no,yes,yes),881).
encodecodeHist(history( no,yes, no, no,yes,yes,yes, no,yes,yes),882).
encodecodeHist(history(yes,yes, no, no,yes,yes,yes, no,yes,yes),883).
encodecodeHist(history( no, no,yes, no,yes,yes,yes, no,yes,yes),884).
encodecodeHist(history(yes, no,yes, no,yes,yes,yes, no,yes,yes),885).
encodecodeHist(history( no,yes,yes, no,yes,yes,yes, no,yes,yes),886).
encodecodeHist(history(yes,yes,yes, no,yes,yes,yes, no,yes,yes),887).
encodecodeHist(history( no, no, no,yes,yes,yes,yes, no,yes,yes),888).
encodecodeHist(history(yes, no, no,yes,yes,yes,yes, no,yes,yes),889).
encodecodeHist(history( no,yes, no,yes,yes,yes,yes, no,yes,yes),890).
encodecodeHist(history(yes,yes, no,yes,yes,yes,yes, no,yes,yes),891).
encodecodeHist(history( no, no,yes,yes,yes,yes,yes, no,yes,yes),892).
encodecodeHist(history(yes, no,yes,yes,yes,yes,yes, no,yes,yes),893).
encodecodeHist(history( no,yes,yes,yes,yes,yes,yes, no,yes,yes),894).
encodecodeHist(history(yes,yes,yes,yes,yes,yes,yes, no,yes,yes),895).
encodecodeHist(history( no, no, no, no, no, no, no,yes,yes,yes),896).
encodecodeHist(history(yes, no, no, no, no, no, no,yes,yes,yes),897).
encodecodeHist(history( no,yes, no, no, no, no, no,yes,yes,yes),898).
encodecodeHist(history(yes,yes, no, no, no, no, no,yes,yes,yes),899).
encodecodeHist(history( no, no,yes, no, no, no, no,yes,yes,yes),900).
encodecodeHist(history(yes, no,yes, no, no, no, no,yes,yes,yes),901).
encodecodeHist(history( no,yes,yes, no, no, no, no,yes,yes,yes),902).
encodecodeHist(history(yes,yes,yes, no, no, no, no,yes,yes,yes),903).
encodecodeHist(history( no, no, no,yes, no, no, no,yes,yes,yes),904).
encodecodeHist(history(yes, no, no,yes, no, no, no,yes,yes,yes),905).
encodecodeHist(history( no,yes, no,yes, no, no, no,yes,yes,yes),906).
encodecodeHist(history(yes,yes, no,yes, no, no, no,yes,yes,yes),907).
encodecodeHist(history( no, no,yes,yes, no, no, no,yes,yes,yes),908).
encodecodeHist(history(yes, no,yes,yes, no, no, no,yes,yes,yes),909).
encodecodeHist(history( no,yes,yes,yes, no, no, no,yes,yes,yes),910).
encodecodeHist(history(yes,yes,yes,yes, no, no, no,yes,yes,yes),911).
encodecodeHist(history( no, no, no, no,yes, no, no,yes,yes,yes),912).
encodecodeHist(history(yes, no, no, no,yes, no, no,yes,yes,yes),913).
encodecodeHist(history( no,yes, no, no,yes, no, no,yes,yes,yes),914).
encodecodeHist(history(yes,yes, no, no,yes, no, no,yes,yes,yes),915).
encodecodeHist(history( no, no,yes, no,yes, no, no,yes,yes,yes),916).
encodecodeHist(history(yes, no,yes, no,yes, no, no,yes,yes,yes),917).
encodecodeHist(history( no,yes,yes, no,yes, no, no,yes,yes,yes),918).
encodecodeHist(history(yes,yes,yes, no,yes, no, no,yes,yes,yes),919).
encodecodeHist(history( no, no, no,yes,yes, no, no,yes,yes,yes),920).
encodecodeHist(history(yes, no, no,yes,yes, no, no,yes,yes,yes),921).
encodecodeHist(history( no,yes, no,yes,yes, no, no,yes,yes,yes),922).
encodecodeHist(history(yes,yes, no,yes,yes, no, no,yes,yes,yes),923).
encodecodeHist(history( no, no,yes,yes,yes, no, no,yes,yes,yes),924).
encodecodeHist(history(yes, no,yes,yes,yes, no, no,yes,yes,yes),925).
encodecodeHist(history( no,yes,yes,yes,yes, no, no,yes,yes,yes),926).
encodecodeHist(history(yes,yes,yes,yes,yes, no, no,yes,yes,yes),927).
encodecodeHist(history( no, no, no, no, no,yes, no,yes,yes,yes),928).
encodecodeHist(history(yes, no, no, no, no,yes, no,yes,yes,yes),929).
encodecodeHist(history( no,yes, no, no, no,yes, no,yes,yes,yes),930).
encodecodeHist(history(yes,yes, no, no, no,yes, no,yes,yes,yes),931).
encodecodeHist(history( no, no,yes, no, no,yes, no,yes,yes,yes),932).
encodecodeHist(history(yes, no,yes, no, no,yes, no,yes,yes,yes),933).
encodecodeHist(history( no,yes,yes, no, no,yes, no,yes,yes,yes),934).
encodecodeHist(history(yes,yes,yes, no, no,yes, no,yes,yes,yes),935).
encodecodeHist(history( no, no, no,yes, no,yes, no,yes,yes,yes),936).
encodecodeHist(history(yes, no, no,yes, no,yes, no,yes,yes,yes),937).
encodecodeHist(history( no,yes, no,yes, no,yes, no,yes,yes,yes),938).
encodecodeHist(history(yes,yes, no,yes, no,yes, no,yes,yes,yes),939).
encodecodeHist(history( no, no,yes,yes, no,yes, no,yes,yes,yes),940).
encodecodeHist(history(yes, no,yes,yes, no,yes, no,yes,yes,yes),941).
encodecodeHist(history( no,yes,yes,yes, no,yes, no,yes,yes,yes),942).
encodecodeHist(history(yes,yes,yes,yes, no,yes, no,yes,yes,yes),943).
encodecodeHist(history( no, no, no, no,yes,yes, no,yes,yes,yes),944).
encodecodeHist(history(yes, no, no, no,yes,yes, no,yes,yes,yes),945).
encodecodeHist(history( no,yes, no, no,yes,yes, no,yes,yes,yes),946).
encodecodeHist(history(yes,yes, no, no,yes,yes, no,yes,yes,yes),947).
encodecodeHist(history( no, no,yes, no,yes,yes, no,yes,yes,yes),948).
encodecodeHist(history(yes, no,yes, no,yes,yes, no,yes,yes,yes),949).
encodecodeHist(history( no,yes,yes, no,yes,yes, no,yes,yes,yes),950).
encodecodeHist(history(yes,yes,yes, no,yes,yes, no,yes,yes,yes),951).
encodecodeHist(history( no, no, no,yes,yes,yes, no,yes,yes,yes),952).
encodecodeHist(history(yes, no, no,yes,yes,yes, no,yes,yes,yes),953).
encodecodeHist(history( no,yes, no,yes,yes,yes, no,yes,yes,yes),954).
encodecodeHist(history(yes,yes, no,yes,yes,yes, no,yes,yes,yes),955).
encodecodeHist(history( no, no,yes,yes,yes,yes, no,yes,yes,yes),956).
encodecodeHist(history(yes, no,yes,yes,yes,yes, no,yes,yes,yes),957).
encodecodeHist(history( no,yes,yes,yes,yes,yes, no,yes,yes,yes),958).
encodecodeHist(history(yes,yes,yes,yes,yes,yes, no,yes,yes,yes),959).
encodecodeHist(history( no, no, no, no, no, no,yes,yes,yes,yes),960).
encodecodeHist(history(yes, no, no, no, no, no,yes,yes,yes,yes),961).
encodecodeHist(history( no,yes, no, no, no, no,yes,yes,yes,yes),962).
encodecodeHist(history(yes,yes, no, no, no, no,yes,yes,yes,yes),963).
encodecodeHist(history( no, no,yes, no, no, no,yes,yes,yes,yes),964).
encodecodeHist(history(yes, no,yes, no, no, no,yes,yes,yes,yes),965).
encodecodeHist(history( no,yes,yes, no, no, no,yes,yes,yes,yes),966).
encodecodeHist(history(yes,yes,yes, no, no, no,yes,yes,yes,yes),967).
encodecodeHist(history( no, no, no,yes, no, no,yes,yes,yes,yes),968).
encodecodeHist(history(yes, no, no,yes, no, no,yes,yes,yes,yes),969).
encodecodeHist(history( no,yes, no,yes, no, no,yes,yes,yes,yes),970).
encodecodeHist(history(yes,yes, no,yes, no, no,yes,yes,yes,yes),971).
encodecodeHist(history( no, no,yes,yes, no, no,yes,yes,yes,yes),972).
encodecodeHist(history(yes, no,yes,yes, no, no,yes,yes,yes,yes),973).
encodecodeHist(history( no,yes,yes,yes, no, no,yes,yes,yes,yes),974).
encodecodeHist(history(yes,yes,yes,yes, no, no,yes,yes,yes,yes),975).
encodecodeHist(history( no, no, no, no,yes, no,yes,yes,yes,yes),976).
encodecodeHist(history(yes, no, no, no,yes, no,yes,yes,yes,yes),977).
encodecodeHist(history( no,yes, no, no,yes, no,yes,yes,yes,yes),978).
encodecodeHist(history(yes,yes, no, no,yes, no,yes,yes,yes,yes),979).
encodecodeHist(history( no, no,yes, no,yes, no,yes,yes,yes,yes),980).
encodecodeHist(history(yes, no,yes, no,yes, no,yes,yes,yes,yes),981).
encodecodeHist(history( no,yes,yes, no,yes, no,yes,yes,yes,yes),982).
encodecodeHist(history(yes,yes,yes, no,yes, no,yes,yes,yes,yes),983).
encodecodeHist(history( no, no, no,yes,yes, no,yes,yes,yes,yes),984).
encodecodeHist(history(yes, no, no,yes,yes, no,yes,yes,yes,yes),985).
encodecodeHist(history( no,yes, no,yes,yes, no,yes,yes,yes,yes),986).
encodecodeHist(history(yes,yes, no,yes,yes, no,yes,yes,yes,yes),987).
encodecodeHist(history( no, no,yes,yes,yes, no,yes,yes,yes,yes),988).
encodecodeHist(history(yes, no,yes,yes,yes, no,yes,yes,yes,yes),989).
encodecodeHist(history( no,yes,yes,yes,yes, no,yes,yes,yes,yes),990).
encodecodeHist(history(yes,yes,yes,yes,yes, no,yes,yes,yes,yes),991).
encodecodeHist(history( no, no, no, no, no,yes,yes,yes,yes,yes),992).
encodecodeHist(history(yes, no, no, no, no,yes,yes,yes,yes,yes),993).
encodecodeHist(history( no,yes, no, no, no,yes,yes,yes,yes,yes),994).
encodecodeHist(history(yes,yes, no, no, no,yes,yes,yes,yes,yes),995).
encodecodeHist(history( no, no,yes, no, no,yes,yes,yes,yes,yes),996).
encodecodeHist(history(yes, no,yes, no, no,yes,yes,yes,yes,yes),997).
encodecodeHist(history( no,yes,yes, no, no,yes,yes,yes,yes,yes),998).
encodecodeHist(history(yes,yes,yes, no, no,yes,yes,yes,yes,yes),999).
encodecodeHist(history( no, no, no,yes, no,yes,yes,yes,yes,yes),1000).
encodecodeHist(history(yes, no, no,yes, no,yes,yes,yes,yes,yes),1001).
encodecodeHist(history( no,yes, no,yes, no,yes,yes,yes,yes,yes),1002).
encodecodeHist(history(yes,yes, no,yes, no,yes,yes,yes,yes,yes),1003).
encodecodeHist(history( no, no,yes,yes, no,yes,yes,yes,yes,yes),1004).
encodecodeHist(history(yes, no,yes,yes, no,yes,yes,yes,yes,yes),1005).
encodecodeHist(history( no,yes,yes,yes, no,yes,yes,yes,yes,yes),1006).
encodecodeHist(history(yes,yes,yes,yes, no,yes,yes,yes,yes,yes),1007).
encodecodeHist(history( no, no, no, no,yes,yes,yes,yes,yes,yes),1008).
encodecodeHist(history(yes, no, no, no,yes,yes,yes,yes,yes,yes),1009).
encodecodeHist(history( no,yes, no, no,yes,yes,yes,yes,yes,yes),1010).
encodecodeHist(history(yes,yes, no, no,yes,yes,yes,yes,yes,yes),1011).
encodecodeHist(history( no, no,yes, no,yes,yes,yes,yes,yes,yes),1012).
encodecodeHist(history(yes, no,yes, no,yes,yes,yes,yes,yes,yes),1013).
encodecodeHist(history( no,yes,yes, no,yes,yes,yes,yes,yes,yes),1014).
encodecodeHist(history(yes,yes,yes, no,yes,yes,yes,yes,yes,yes),1015).
encodecodeHist(history( no, no, no,yes,yes,yes,yes,yes,yes,yes),1016).
encodecodeHist(history(yes, no, no,yes,yes,yes,yes,yes,yes,yes),1017).
encodecodeHist(history( no,yes, no,yes,yes,yes,yes,yes,yes,yes),1018).
encodecodeHist(history(yes,yes, no,yes,yes,yes,yes,yes,yes,yes),1019).
encodecodeHist(history( no, no,yes,yes,yes,yes,yes,yes,yes,yes),1020).
encodecodeHist(history(yes, no,yes,yes,yes,yes,yes,yes,yes,yes),1021).
encodecodeHist(history( no,yes,yes,yes,yes,yes,yes,yes,yes,yes),1022).
encodecodeHist(history(yes,yes,yes,yes,yes,yes,yes,yes,yes,yes),1023).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of private predicates and functions

:- end_module gl.givetake.strategy.encodecode.

%%% Local Variables: 
%%% mode: mercury
%%% mode: flyspell-prog
%%% ispell-local-dictionary: "british"
%%% End:
