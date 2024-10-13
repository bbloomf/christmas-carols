\version "2.14.2"
\include "util.ly"
\version "2.14.2"
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Angelus ad virginem"}}
  poet = \markup\oldStyleNum"13th Century"
  composer = \markup\oldStyleNum"Anonymous, 13th Century"
  tagline = \markup { "from" \italic "cpdl.org"}
}
\paper {
  %print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
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
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #050
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
global = {
  \key g \major
  \time 6/8
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
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
  An -- ge -- lus ad vir -- gi -- nem
      Sub -- in -- trans in con -- cla -- ve.
  Vir -- gi -- nis for -- mi -- di -- nem
      De -- mul -- cens in -- quit “A -- ve.”
  A -- ve re -- gi -- na vir -- gi -- num,
  Cæ -- li ter -- ræ -- que do -- mi -- num
      Con -- ci -- pi -- es
      Et pa -- ri -- es
      In -- tac -- ta,
      Sa -- lu -- tem ho -- mi -- num.
      Tu por -- ta cæ -- li fac -- ta
      Me -- del -- la cri -- mi -- num.
      
  \set stanza = #"3. "
  Ad hæc vir -- go no -- bi -- lis
      Re -- spon -- dens in -- quit e -- i;
  An -- cil -- la sum hu -- mi -- lis
      Om -- ni -- po -- ten -- tis De -- i.
  Ti -- bi cæ -- les -- ti nun -- ti -- o,
  Tan -- ta se -- cre -- ti con -- sci -- o,
      Con -- sen -- ti -- ens
      Et cu -- pi -- ens
      Vi -- de -- re
      fac -- tum quod au -- di -- o,
      Pa -- ra -- ta sum pa -- re -- re
      De -- i con -- si -- li -- o.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Quo -- mo -- do con -- ci -- pe -- rem,
      quæ vi -- rum non co -- gno -- vi?
  Qua -- li -- ter in -- frin -- ge -- rem,
      quæ fir -- ma men -- te vo -- vi?
  ‘Spi -- ri -- tus sanc -- ti gra -- ti -- a
  Per -- fi -- ci -- et hæc om -- ni -- a;
      Ne ti -- me -- as,
      sed gau -- de -- as,
      se -- cu -- ra,
      quod cas -- ti -- mo -- ni -- a
      Ma -- ne -- bit in te pu -- ra
      De -- i po -- ten -- ti -- a.’
      
  \set stanza = #"4. "
  An -- ge -- lus dis -- pa -- ru -- it
      Et sta -- tim pu -- el -- la -- ris
  U -- te -- rus in -- tu -- mu -- it
      Vi par -- tus sa -- lu -- ta -- ris.
  Qui, cir -- cum -- da -- tus u -- te -- ro
  No -- vem men -- si -- um nu -- me -- ro,
      Hinc Ex -- i -- it
      Et in -- i -- it
      Con -- flic -- tum,
      Af -- fi -- gens hu -- me -- ro
      Cru -- cem, qua de -- dit ic -- tum
      Hos -- ti mor -- ti -- fe -- ro.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \repeat unfold 74 \skip1
  \set stanza = #"5. "
  E -- ia Ma -- ter Do -- mi -- ni,
      Quæ pa -- cem red -- di -- di -- sti
  An -- ge -- lis et ho -- mi -- ni,
      Cum Chri -- stum ge -- nu -- i -- sti;
  Tu -- um ex -- o -- ra fi -- li -- um
  Ut se no -- bis pro -- pi -- ti -- um
      Ex -- hi -- be -- at,
      Et de -- le -- at
      Pec -- ca -- ta;
      Præ -- stans au -- xi -- li -- um
      Vi -- ta fru -- i be -- a -- ta
      Post hoc ex -- si -- li -- um.
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.4))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold 2 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold 2 \bassMusic >> }
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
    \tempo 4 = 135
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
