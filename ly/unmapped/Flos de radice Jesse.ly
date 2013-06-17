\version "2.14.2"
\include "util.ly"
\header {
  tagline = ""%\markup { "from" \concat{\italic "HymnsAndCarolsOfChristmas.com"}}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Flos de radice Jesse"}}
    composer = \markup\oldStyleNum"Arranged by Michael Praetorius (1571–1621)"
    %tagline = \markup { "from" \italic {HymnsAndCarolsOfChristmas.com}}
  }
\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -12)
       (stretchability . 100))
  %markup-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -5)
  %     (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 100))
%{IF_LESSER
  markup-system-spacing #'stretchability = 50
  top-markup-spacing #'stretchability = 30
  last-bottom-spacing #'stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #092
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }









%IF_NOT_LESSER
global = {
  \key g \major
  \time 4/4
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  d2 d4 d |
  e d d2 |
  b c |
  b4 a2 g4( |
  
  g) fis g2 |
  d' d4 d |
  e d d2 |
  b c |
  
  b4 a2 g4( |
  g) fis g2 |
  b4\rest b4 a fis |
  g e d2 |
  
  b'4\rest d4 d d |
  e d d2 |
  b c |
  b4 a2 g4( |
  g) fis g2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  b2 b4 g |
  g g fis2 |
  e e |
  d4 d e4.( b8 |
  
  d4) d4 d2 |
  b'2 b4 g |
  g g fis2 |
  e e |
  
  d4 d e4.( b8 |
  d4) d d2 |
  s4 g e d |
  d cis d2 |
  
  s4 fis a g |
  g g fis2 |
  e e |
  g4 e fis2( |
  d4) d d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Flos de ra -- di -- ce Jes -- se, est na -- tus ho -- di -- e.
  Quem no -- bis jam ad -- es -- se, læ -- ta -- mur u -- ni -- ce.
  Flos il -- le Je -- sus est.
  Ma -- ri -- a Vir -- go ra -- dix de qua flos or -- tus est.
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  Hunc I -- sa -- ias flo -- rem, præ -- sa -- gi -- is ce -- ci -- nit.
  Ad e -- jus nos a -- mo -- rem, Na -- scen -- tis al -- li -- cit.
  Flos vir -- gam su -- pe -- rat
  cœ -- li ter -- ræ -- que ci -- ves, Flos il -- le re -- cre -- at.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Est cam -- pi flos pu -- di -- ci, est flos con -- val -- li -- um.
  Pul -- crum -- que pot -- est di -- ci, in spi -- nis li -- li -- um.
  O -- do -- ris op -- ti -- mi;
  vel so -- li quod -- vis ce -- dit a -- ro -- ma no -- mi -- ni.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
%  Hic su -- o flos o -- do -- re, fi -- de -- les at -- tra -- hit. 
%  Di -- vi -- no mox a -- mo -- re, at -- trac -- tos im -- bu -- it.
%  O flos o gra -- ti -- a:
%  ad te sus -- pi -- ro, de te me sa -- ti -- a.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  d2 d4 b |
  c b a2 |
  g g |
  b4 d c( b~ |
  
  b) a b2 |
  d d4 b |
  c b a2 |
  g g |
  
  b4 d c( b~ |
  b) a b2 |
  s4 d4 c b |
  b a fis8[( g] a4) |
  
  s4 a4 a b |
  c b a2 |
  gis a |
  d4 c b2( |
  b4) a b2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'2 g4 g |
  c g d2 |
  e c |
  g'4 fis e2 |
  
  d2 g |
  g g4 g |
  c g d2 |
  e c |
  
  g'4 fis e2 |
  d g, |
  d'4\rest g a b |
  g a d,8[( e] fis4) |
  
  d4\rest d fis g |
  c, g' d2 |
  e a, |
  b4 c d2 |
  d g \bar "|."
}
bassWords = \lyricmode {
  \repeat unfold 11 { \skip 1 }
  -di -- e.
  \repeat unfold 11 { \skip 1 }
  -ni -- ce.
  \repeat unfold 17 { \skip 1 }
  -tus est.
}
bassWordsII = \lyricmode {
  \repeat unfold 11 { \skip 1 }
  -ci -- nit.
  \repeat unfold 11 { \skip 1 }
  -li -- cit.
  \repeat unfold 17 { \skip 1 }
  -cre -- at.
}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
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
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWordsII
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.6 20)))
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
%6.14g    \context { \Lyrics \override LyricText #'font-size = #0.6 }
%6x9 \context {\Lyrics\override LyricText #'font-size = #1.2 }
  }
  
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
