\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Corde Natus"}}
  poet = \markup\oldStyleNum"Marcus Aurelius Clemens Prudentius (348–413?)"
  composer = \markup\concat{\italic"Divinum Mysterium" \oldStyleNum", 13th Century Melody"}
  tagline = \markup\center-column{
    \concat{"from " \italic"Great Hymns of the Church Compiled by the Late Right Reverend John Freeman Young" \oldStyleNum", 1887,"}
    \concat{"via " \italic"HymnsAndCarolsOfChristmas.com"}
  }
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #122
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
%6.14 #(set-global-staff-size 14) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14 20))) }
%6x9 #(set-global-staff-size 14.5) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.5 20))) }
global = {
  \key f \major
  \time 12/2
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 3 {
    f2. g4 a2 bes a g a2.( g4) f\breve |
    a2 bes c d c( a1) bes2 c\breve \bar "||"
    
    d2. e4 f2 c c bes a2.( g4) f\breve |
    d2. e4 f2 g f( d1) e2 f\breve \bar "||"
    
    f2. g4 a2 bes a g c1 d\breve |
    \partial 2*6 c2( a) bes c( f,) e |
    
    \partial 2*8 d2 e f2.( d4 c\breve) |
    f2. g4 a2 c a f g1( f\breve)
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f2. f4 f2 f f f f1 f\breve |
  f2 f f f e( f1) f2 e\breve \bar "||"
  
  f2. g4 f2 f f f4( g) f2( e) d\breve |
  bes2. c4 c2 d c1 bes a\breve \bar "||"
  
  f'2. f4 f2 f f g f1 f\breve |
  e2( f) f f( c) c |
  
  bes2 bes a2.( bes4 a\breve) |
  f'2. f4 f2 e e f e1( f\breve)
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set associatedVoice = "sopranos"
  \set stanza = #"1. "
  Cor -- de na -- tus ex pa -- rén -- tis
  An -- te mun -- di~ex -- ór -- di -- um
  A et O co -- gno -- mi -- ná -- tus,
  ip -- se fons et cláu -- su -- la
  Om -- ni -- um quæ sunt, fu -- é -- runt,
  quæ -- que post fu -- tú -- ra sunt. __
  Sæ -- cu -- ló -- rum sǽ -- cu -- lis. __
  
  \set stanza = #"4. "
  O be -- á -- tus or -- tus il -- le,
  vir -- go cum pu -- ér -- pe -- ra
  E -- di -- dit nos -- tram sa -- lú -- tem,
  fe -- ta Sanc -- to Spi -- ri -- tu,
  Et pu -- er red -- émp -- tor or -- bis
  os sa -- crá -- tum pró -- tu -- lit. __
  Sæ -- cu -- ló -- rum sǽ -- cu -- lis. __
  
  
  \set stanza = #"7. "
  Mac -- te ju -- dex mor -- tu -- ó -- rum,
  mac -- te rex vi -- vén -- ti -- um,
  Dex -- ter in Pa -- rén -- tis ar -- ce
  qui clu -- is vir -- tú -- ti -- bus,
  Om -- ni -- um ven -- tú -- rus in -- de
  jus -- tus ul -- tor crí -- mi -- num. __
  Sæ -- cu -- ló -- rum sǽ -- cu -- lis. __
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set associatedVoice = "sopranos"
  \set stanza = #"2. "
  Ip -- se jus -- sit et cre -- á -- ta,
  di -- xit ip -- se~et fac -- ta sunt,
  Ter -- ra, cæ -- lum, fos -- sa pon -- ti,
  tri -- na re -- rum má -- chi -- na,
  Quæ -- que~in his vi -- gent sub al -- to
  so -- lis et lu -- næ glo -- bo. __
  Sæ -- cu -- ló -- rum sǽ -- cu -- lis. __
  
  \set stanza = #"5. "
  Psal -- lat al -- ti -- tú -- do cæ -- li,
  psal -- lant om -- nes án -- ge -- li,
  Quid -- quid est vir -- tú -- tis us -- quam
  psal -- lat in lau -- dem De -- i,
  Nul -- la lin -- guá -- rum si -- lés -- cat,
  vox et om -- nis cón -- so -- net. __
  Sæ -- cu -- ló -- rum sǽ -- cu -- lis. __
  
  \set stanza = #"8. "
  Te se -- nes et te ju -- vén -- tus,
  par -- vu -- ló -- rum te cho -- rus,
  Tur -- ba ma -- trum, vir -- gi -- núm -- que,
  sím -- pli -- ces pu -- él -- lu -- læ,
  Vo -- ce con -- cór -- des pu -- dí -- cis
  pér -- stre -- pant con -- cén -- ti -- bus. __
  Sæ -- cu -- ló -- rum sǽ -- cu -- lis. __
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set associatedVoice = "sopranos"
  \set stanza = #"3. "
  Cór -- po -- ris for -- mam ca -- dú -- ci,
  mem -- bra mor -- ti~ob -- nó -- xi -- a
  Ind -- u -- it, ne gens per -- í -- ret
  pri -- mo -- plás -- ti~ex gér -- mi -- ne,
  Mér -- se -- rat quem lex pro -- fún -- do
  no -- xi -- á -- lis tár -- ta -- ro. __
  Sæ -- cu -- ló -- rum sǽ -- cu -- lis. __
  
  
  \set stanza = #"6. "
  Ec -- ce, quem va -- tes ve -- tús -- tis
  con -- ci -- né -- bant sǽ -- cu -- lis,
  Quem pro -- phe -- tá -- rum fi -- dé -- les
  pá -- gi -- næ spo -- pón -- de -- rant,
  E -- mi -- cat pro -- mís -- sus o -- lim;
  cunc -- ta con -- láu -- dent e -- um. __
  Sæ -- cu -- ló -- rum sǽ -- cu -- lis. __
  
  \set stanza = #"9. "
  Ti -- bi, Chris -- te, sit cum Pa -- tre
  há -- gi -- o -- que Pnéu -- ma -- te
  Hym -- nus, de -- cus, laus per -- én -- nis,
  gra -- ti -- á -- rum ác -- ti -- o,
  Ho -- nor, vir -- tus, vic -- tó -- ri -- a,
  re -- gnum æ -- ter -- ná -- li -- ter. __
  Sæ -- cu -- ló -- rum sǽ -- cu -- lis. __
}
altoWordsIV = \lyricmode {
  \set associatedVoice = "sopranos"
}
altoWordsV = \lyricmode {
  \set associatedVoice = "sopranos"
}
altoWordsVI = \lyricmode {
  \set associatedVoice = "sopranos"
}
altoWordsVII = \lyricmode {
  \set associatedVoice = "sopranos"
}
altoWordsVIII = \lyricmode {
  \set associatedVoice = "sopranos"
}
altoWordsIX = \lyricmode {
  \set associatedVoice = "sopranos"
}
tenorMusic = \relative c' {
  a2. bes4 c2 d c bes c2.( bes4) a\breve |
  c2 bes a bes g( f1) f2 g\breve \bar "||"
  
  d'2. c4 c2 f4( e) d2 d c2.( bes4) a\breve |
  bes2. g4 f2 f f1. c2 c\breve \bar "||"
  
  a'2. bes4 c2 d c2. bes4 bes2( a) bes\breve |
  g2( f2.) g4 a2( f) f |
  
  f2 g f1~f\breve |
  a2. bes4 c2 c c a bes1( a\breve)
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f2. f4 f2 f f f f1 f\breve |
  f2 e4( d) c2 bes c( f1) d2 c\breve \bar "||"
  
  bes'2. bes4 a2 a bes g c,1 d\breve |
  bes2. bes4 a2 bes a( bes1) bes2 f\breve \bar "||"
  
  f'2. f4 f2 f f e f1 bes,\breve |
  c2( f) d a( a) a |
  
  bes2 bes f1~ f\breve |
  f'2. f4 f2 a,4( bes) c2 d c1( f\breve)
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold3\sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold3\altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIX"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIX
    \new Lyrics = "altosVIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVIII
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVII
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold3\tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold3\bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
%6.14 \context {\Lyrics \override LyricText.font-size = #0.5 }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

