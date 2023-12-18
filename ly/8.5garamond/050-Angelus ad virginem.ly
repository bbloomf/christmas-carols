\version "2.24.0"
\include "util.ly"
\version "2.24.0"
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Angelus ad virginem"}}
  poet = \markup\oldStyleNum"13th Century"
  composer = \markup\oldStyleNum"Anonymous, 13th Century"
  tagline = \markup { "from" \italic "cpdl.org"}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #050
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
global = {
  \key g \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  \repeat volta 2 {
    d4 b8 d4 d8 |
    c4 b8 a4 a8 |
    b4 g8 a[ g] fis |
    g4. g |
    d'4 b8 d4 d8 |
    
    c4 b8 a4 a8 |
    b4 g8 a[ g] fis |
    g4. g |
    a c4 c8 |
    b4 g8 a[ b] a |
    g4. a |
    
    %page2
    c4 c8 b4 g8 |
    a[ g] a d,4. |
    e8( fis4) g fis8 |
    e4 d8 g4 e8 |
    g[ a] b b[ a g] |
    a4 d,8 e4 f8 |
    
    e[ d] c d4. |
    e8( fis4) g e8 |
    g[ a] b b[ a g] |
    a4 d,8 e4 f8 |
    e[ d] c d4. \break
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  b4 g8 b4 g8 |
  e8[ fis] g8 fis4 fis8 |
  g4 e8 d4 d8 |
  b[ d c] b4. |
  
  b'4 g8 b4 g8 |
  e8[ fis] g8 fis4 fis8 |
  g4 e8 d4 d8 |
  b[ c a] b4. |
  
  fis'4. e4 e8 |
  d4 d8 fis4 fis8 |
  e4. fis |
  e4 e8 g4 e8 |
  fis4 fis8 d4. |
  
  b8( d4) b4 d8 |
  c4 b8 e4 e8 |
  e4 e8 d4( e8) |
  fis4 d8 c4 c8 |
  c4 c8 d4. |
  
  c8( d4) e4 c8 |
  e8[ fis] g g[ fis e] |
  fis4 d8 c4 c8 |
  c4 a8 b4.
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  An -- ge -- lus ad vír -- gi -- nem
      Sub -- ín -- trans in con -- clá -- ve.
  Vír -- gi -- nis for -- mí -- di -- nem
      De -- múl -- cens in -- quit: “A -- ve!
  A -- ve re -- gí -- na vír -- gi -- num!
  Cæ -- li ter -- rǽ -- que Dó -- mi -- num
      Con -- cí -- pi -- es
      Et pá -- ri -- es
      In -- tác -- ta,
      Sa -- lú -- tem hó -- mi -- num.
      Tu por -- ta cæ -- li fac -- ta
      Me -- dél -- la crí -- mi -- num.”
      
  \set stanza = #"3. "
  Ad hæc vir -- go nó -- bi -- lis
      Re -- spón -- dens in -- quit e -- i:
  “An -- cíl -- la sum hú -- mi -- lis
      Om -- ni -- pot -- én -- tis De -- i.
  Ti -- bi cæ -- lés -- ti nún -- ti -- o,
  Tan -- ta se -- cré -- ti cón -- sci -- o,
      Con -- sén -- ti -- ens
      Et cú -- pi -- ens
      Vi -- dé -- re
      fac -- tum quod áu -- di -- o,
      Pa -- rá -- ta sum pa -- ré -- re
      De -- i con -- sí -- li -- o.”
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  “Quó -- mo -- do con -- cí -- pe -- rem,
      quæ vi -- rum non co -- gnó -- vi?
  Quá -- li -- ter in -- frín -- ge -- rem,
      quæ fir -- ma men -- te vo -- vi?”
  “Spí -- ri -- tus Sanc -- ti grá -- ti -- a
  Per -- fí -- ci -- et hæc óm -- ni -- a;
      Ne tí -- me -- as,
      sed gáu -- de -- as,
      se -- cú -- ra,
      quod cas -- ti -- mó -- ni -- a
      Ma -- né -- bit in te pu -- ra
      De -- i pot -- én -- ti -- a.”
      
  \set stanza = #"4. "
  An -- ge -- lus dis -- pá -- ru -- it
      Et sta -- tim pu -- el -- lá -- ris
  U -- te -- rus in -- tú -- mu -- it
      Vi par -- tus sa -- lu -- tá -- ris.
  Qui, cir -- cúm -- da -- tus ú -- te -- ro
  No -- vem mén -- si -- um nú -- me -- ro,
      Hinc éx -- i -- it
      Et ín -- i -- it
      Con -- flíc -- tum,
      Af -- fí -- gens hú -- me -- ro
      Cru -- cem, qua de -- dit ic -- tum
      Hos -- ti mor -- tí -- fe -- ro.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \repeat unfold 74 \skip1
  \set stanza = #"5. "
  E -- ia Ma -- ter Dó -- mi -- ni,
      Quæ pa -- cem red -- di -- dís -- ti
  An -- ge -- lis et hó -- mi -- ni,
      Cum Chris -- tum ge -- nu -- ís -- ti;
  Tu -- um ex -- ó -- ra fí -- li -- um
  Ut se no -- bis pro -- pí -- ti -- um
      Ex -- hí -- be -- at,
      Et dé -- le -- at
      Pec -- cá -- ta;
      Præ -- stans au -- xí -- li -- um
      Vi -- ta fru -- i be -- á -- ta
      Post hoc ex -- sí -- li -- um.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  g4 d'8 g,4 b8 |
  c4 d8 d4 d8 |
  b4 b8 fis8[ g] a8 |
  g[ b a] g4. |
  
  g4 d'8 g,4 b8 |
  c4 d8 d4 d8 |
  b4 b8 fis8[ g] a8 |
  g[ e fis] g4. |
  
  d'4. g,4 g8 |
  b4 b8 a4 a8 |
  b4. d |
  a4 a8 b4 b8 |
  d4 a8 fis4. |
  
  g8( a4) g4 b8 |
  g4 g8 b4 g8 |
  b4 g8 b[ d b] |
  a4 d8 g,4 a8 |
  g4 e8 fis4. |
  
  g8( a4) c4 g8 |
  b[ d] b b[ d b] |
  a4 d8 g,4 a8 |
  g4 e8 g4.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g4 g8 g4 g8 |
  c4 g8 d'4 d8 |
  e4 e8 d4 d8 |
  g,4. g |
  
  g4 g8 g4 g8 |
  c4 g8 d'4 d8 |
  e4 e8 d4 d8 |
  g,4. g |
  
  d'4. c4 c8 |
  g'4 g8 d4 d8 |
  e4. d |
  a4 a8 e'4 e8 |
  d4 d8 d4. |
  
  e8( d4) e4 b8 |
  c4 g'8 e4 e8 |
  e4 e8 g[ fis e] |
  d4 d8 c4 f,8 |
  c'[ d] e d4. |
  
  e8( d4) c4 c8 |
  g'[ fis] e e[ fis g] |
  d4 d8 c4 f,8 |
  c'[ b] a g4.
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
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold 2 \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold 2 \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.4))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold 2 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold 2 \bassMusic >> }
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

