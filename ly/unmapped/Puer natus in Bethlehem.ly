\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Puer natus in Bethlehem"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #10.5 "(A Babe is Born in Bethlehem)"}}
  poet = \markup\oldStyleNum"14th century or earlier"
  composer = \markup \concat{"From " \italic "Piæ Cantiones" \oldStyleNum"*, 1582"}
  tagline = \markup { \center-column { \concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"} \justify {*In \italic Piæ \italic Cantiones only a tenor and bass part were given, and in \italic The \italic Cowley \italic Carol \italic Book (and here), the bass line from \italic Piæ \italic Cantiones is found in the soprano, while the tenor is retained as the tenor.}}}
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
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
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #154
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
global = {
  \key f \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 4 {
    \partial 4 g'4 |
    g2 g4 |
    a2 a4 |
    bes2 g4 |
    \partial 2 f2 \bar "||"
    \partial 4 f4 |
    bes2( c4 |
    
    bes2) a4 |
    \partial 2 bes2\fermata \bar "||" \break
    \partial 4 bes4 |
    bes2 bes4 |
    a2 g4 |
    g2 f4 |
    \partial 2 g2 \bar "||"
    
    \partial 4 g4 |
    f2( g4 |
    a2 f4 |
    bes2 a4 |
    g2) f4 |
    \partial 2 g2\fermata
  }
  \break
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  g'4 |
  bes,2 d4 |
  f2 f4 |
  f2 ees4 |
  d2 \bar "||"
  d4 |
  f2( f4 |
  
  f2) f4 |
  d2 \bar "||"
  d4 |
  g2 f4 |
  f2 d4 |
  ees2 d4 |
  bes2 \bar "||"
  
  d4 |
  d2.( |
  f |
  f |
  d2) d4 |
  d2
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Pu -- er na -- tus in Beth -- le -- hem,
  Al -- le -- lu -- ia.
  Un -- de gau -- det Je -- ru -- sa -- lem,
  Al -- le -- lu -- ia.
  
  \set stanza = #"5. "
  Si -- ne ser -- pen -- tis vul -- ne -- re,
  Al -- le -- lu -- ia.
  De nos -- tro ve -- nit san -- gui -- ne,
  Al -- le -- lu -- ia.
  
  \set stanza = #"9. "
  Co -- gno -- vit bos et a -- si -- nus
  Al -- le -- lu -- ia.
  Quod Pu -- er e -- rat Do -- mi -- nus.
  Al -- le -- lu -- ia.
  
  \set stanza = #"12. "
  In -- tran -- tes do -- mum in -- vi -- cem,
  Al -- le -- lu -- ia.
  Na -- tum sa -- lu -- tant Ho -- mi -- nem,
  Al -- le -- lu -- ia.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  As -- sum -- psit car -- nem ho -- mi -- nis,
  Al -- le -- lu -- ia.
  Ver -- bum Pa -- tris al -- tis -- si -- mi,
  Al -- le -- lu -- ia.
  
  
  \set stanza = #"6. "
  In car -- ne no -- bis si -- mi -- lis,
  Al -- le -- lu -- ia.
  Pec -- ca -- to sed dis -- si -- mi -- lis,
  Al -- le -- lu -- ia.
  
  \set stanza = #"10. "
  Et an -- ge -- lus pas -- to -- ri -- bus,
  Al -- le -- lu -- ia.
  Re -- ve -- lat Quis sit Do -- mi -- nus,
  Al -- le -- lu -- ia.
  
  
  \set stanza = #"13. "
  In hoc na -- ta -- li gau -- di -- o,
  Al -- le -- lu -- ia.
  Be -- ne -- di -- ca -- mus Do -- mi -- no,
  Al -- le -- lu -- ia.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Per Ga -- bri -- e -- lis nun -- ti -- um,
  Al -- le -- lu -- ia.
  Vir -- go con -- ce -- pit Fi -- li -- um,
  Al -- le -- lu -- ia.
  
  \set stanza = #"7. "
  Tam -- quam spon -- sus de tha -- la -- mo,
  Al -- le -- lu -- ia.
  Pro -- ces -- sit ma -- tris u -- te -- ro,
  Al -- le -- lu -- ia.
  
  \set stanza = #"11. "
  Ma -- gi de lon -- ge ve -- ni -- unt,
  Al -- le -- lu -- ia.
  Au -- rum, thus, myr -- rham of -- fe -- runt.
  Al -- le -- lu -- ia.
  
  \set stanza = #"14. "
  Lau -- de -- tur sanc -- ta Tri -- ni -- tas,
  Al -- le -- lu -- ia.
  De -- o di -- ca -- mus gra -- ti -- as,
  Al -- le -- lu -- ia.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  De ma -- tre na -- tus vir -- gi -- ne,
  Al -- le -- lu -- ia.
  Si -- ne vi -- ri -- li se -- mi -- ne,
  Al -- le -- lu -- ia.
  
  \set stanza = #"8. "
  Hic ja -- cet in præ -- se -- pi -- o,
  Al -- le -- lu -- ia.
  Qui re -- gnat si -- ne ter -- mi -- no.
  Al -- le -- lu -- ia.
  
  
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  g4 |
  g2 g4 |
  c2 c4 |
  bes2 bes4 |
  bes2 \bar "||"
  bes4 |
  d2( f4 |
  
  d2) c4 |
  bes2 \bar "||"
  bes4 |
  d2 d4 |
  c2 bes4 |
  c2 a4 |
  g2 \bar "||"
  
  g4 |
  d'2.( |
  c |
  d2 c4 |
  bes2) a4 |
  g2
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'4 |
  g2 g4 |
  f2 f4 |
  d2 ees4 |
  bes'2 \bar "||"
  bes4 |
  bes2( a4 |
  
  bes2) f4 |
  bes,2 \bar "||"
  bes'4 |
  g2 d4 |
  f2 g4 |
  c,2 d4 |
  g,2 \bar "||"
  
  g'4 |
  bes2.( |
  f d2 f4 |
  g2) d4 |
  g,2\fermata
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
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold4\sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold4\altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold4\tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold4\bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \midi {
    \tempo 4 = 150
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
