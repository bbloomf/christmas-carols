\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Alma Redemptoris Mater"}}
  composer = \markup\oldStyleNum"Giovanni Pierluigi da Palestrina (c. 1525–1594)"
  tagline = ""
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
       (padding . -3)
       (stretchability . 100))
%  top-markup-spacing =
%    #'((basic-distance . 0)
%       (minimum-distance . 0)
%       (padding . 0)
%       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
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
global = {
  \key ees \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner.style = #'none
}

sopMusic = \relative c' {
	r1 |
  bes'2 bes |
  ees,4 f g8[ aes bes g]( |
  aes2 f) |
  ees4 bes'2 bes4 |
  bes bes ees2 |
  d2 bes4 c |
  
  aes2 g4 ees' |
  d ees c2 |
  bes2 r4 bes |
  bes2 g4 f |
  ees2 d4 d'~ |
  d d d d |
  
  ees2 d4 c~ |
  c8[ bes]~ bes2 a4 |
  bes1 \bar"||"
  ees2. d4 |
  c bes c2 |
  bes4 bes c2 |
  bes4 g f2 |
  g ees |
  f g |
  
  %page2
  g4 aes2 g4 |
  f2 g4 ees'~ |
  ees d c2 |
  bes1 |
  r4 ees2 d4 |
  c4. c8 bes4 c~ |
  c8 bes bes2 a4 |
  
  bes bes bes bes~ |
  bes aes g ees~ |
  ees8[ f g ees]( f2) |
  g1 \bar"||"
  ees2 f |
  g aes |
  f1 |
  g4.( aes8 bes2) |
  r2 r4 bes~ |
  
  bes a bes ees, |
  f g aes2~ |
  aes4( g f2) |
  ees2 r4 ees'~ |
  ees d ees c |
  des2 c |
  bes1 |
  bes \bar"|."
}
sopWords = \lyricmode {
  Red -- emp -- tó -- ris Ma -- ter, quæ pér -- vi -- a cæ -- li por -- ta ma -- nes,
  Et stel -- la ma -- ris, suc -- cúr -- re ca -- dén -- ti,
  súr -- ge -- re qui cu -- rat pó -- pu -- lo:
  Tu quæ ge -- nu -- ís -- ti, na -- tú -- ra mi -- rán -- te, tu -- um sanc -- tum Ge -- ni -- tó -- rem:
  Vir -- go pri -- us ac post -- é -- ri -- us, 
    ac post -- é -- ri -- us, 
  Ga -- bri -- é -- lis ab o -- re
  su -- mens il -- lud A -- ve, __ pec -- ca -- tó -- rum mi -- se -- ré -- re,
    pec -- ca -- tó -- rum mi -- se -- ré -- re.
}

altoMusic = \relative c' {
  r1 |
  g'2 g |
  g4 f ees8[ f g ees]( |
  f)[ ees]~ ees2( d4) |
  ees4 g2 g4 |
  g g bes2 |
  bes4 f g ees~ |
  
  ees8[ d16 c]( d4) ees bes' |
  bes bes aes2 |
  g2 r4 g |
  g2 ees4 d |
  c2 f4 bes~ |
  bes bes bes bes |
  
  bes2. aes4 |
  g2( f4.) f8 |
  f1 \bar"||"
  bes2. bes4 |
  g g aes2 |
  g4 g g2 |
  g4 ees d2 |
  ees r4 ees~ |
  ees d ees2 |
  
  %page2
  ees4 ees2 ees4 |
  c2 ees4 ees8[( f] |
  g[ aes] bes2) aes4 |
  g2. f4 |
  r4 ees8([ f] g[ aes] bes4)~ |
  bes aes g( f8[ ees] |
  g4) f f2 |
  
  r4 g g g~ |
  g f ees g~ |
  g8[( f] ees2 d4) |
  ees1 \bar"||"
  bes2 d |
  ees ees4 ees~ |
  ees( d8[ c] d2) |
  ees4 g2( f8[ ees] |
  f2.) f4 |
  
  g4 f ees4.( d8 |
  c4) bes aes4.( bes8 |
  c[ d] ees2 d4) |
  ees4 g2( f8[ ees] |
  g4) f ees8[ f g aes]( |
  bes2) ees,4 f~ |
  f ees2 d4 |
  ees1 \bar"|."
}
altoWords = \lyricmode {
  Red -- emp -- tó -- ris Ma -- ter, quæ pér -- vi -- a cæ -- li por -- ta ma -- nes,
  Et stel -- la ma -- ris, suc -- cúr -- re ca -- dén -- ti,
  súr -- ge -- re qui cu -- rat pó -- pu -- lo:
  Tu quæ ge -- nu -- ís -- ti, na -- tú -- ra mi -- rán -- te, tu -- um sanc -- tum Ge -- ni -- tó -- rem:
  Vir -- go pri -- us ac __ post -- é -- ri -- us, 
  Ga -- bri -- é -- lis ab o -- re
  su -- mens il -- lud A -- ve, pec -- ca -- tó -- rum mi -- se -- ré -- re,
    pec -- ca -- tó -- rum mi -- se -- ré -- re.
}

tenorMusic = \relative c' {
  ees,4( g8[ aes bes c]) bes4 |
  ees2 ees |
  bes4 bes bes2( |
  c) bes |
  r4 ees2 ees4 |
  ees ees g2 |
  f4 d ees2 |
  
  f2 bes,4 g' |
  f g ees2 |
  ees r4 ees |
  ees2 bes4 bes |
  g( a) bes f'~ |
  f f f f |
  
  g2 f4.( ees8 |
  d[ c16 d] ees4) c4. c8 |
  d1 \bar"||"
  g2. f4 |
  ees ees ees2 |
  ees4 ees ees2 |
  ees4 bes bes2 |
  bes g |
  bes bes |
  
  %page2
  c4 c2 bes4 |
  aes2 g |
  r1 |
  r4 ees'2 d4 |
  c2 bes |
  r2 ees~ |
  ees4 d c4. c8 |
  
  bes4 ees ees ees~ |
  ees c c2 |
  bes1 |
  bes \bar"||"
  g2 bes |
  bes c |
  bes1 |
  bes4 ees2( d8[ c] |
  d4) c d2 |
  
  ees4 c bes aes8([ bes] |
  c[ d] ees2 d4) |
  ees2 r |
  bes2. a4 |
  bes2 ees, |
  f4 g aes2~ |
  aes4( g f2) |
  g1 \bar"|."
}

tenorWords = \lyricmode {
  Al -- ma Red -- emp -- tó -- ris Ma -- ter, quæ pér -- vi -- a cæ -- li por -- ta ma -- nes,
  Et stel -- la ma -- ris, suc -- cúr -- re ca -- dén -- ti,
  súr -- ge -- re qui cu -- rat __ pó -- pu -- lo:
  Tu quæ ge -- nu -- ís -- ti, na -- tú -- ra mi -- rán -- te, tu -- um sanc -- tum Ge -- ni -- tó -- rem:
  Vir -- go pri -- us ac __ post -- é -- ri -- us,
  Ga -- bri -- é -- lis ab o -- re
  su -- mens il -- lud A -- ve, pec -- ca -- tó -- rum mi -- se -- ré -- re,
    pec -- ca -- tó -- rum mi -- se -- ré -- re.
}

bassMusic = \relative c' {
  r1 |
  ees,2 ees |
  ees4 d ees2( |
  aes,) bes |
  ees2. ees4 |
  ees ees ees8[ f g aes]( |
  bes4) bes g aes |
  
  f2 ees4 ees |
  bes' g aes2 |
  ees r4 ees |
  ees2 ees4 bes |
  c2 bes |
  bes'4. bes8 bes4 bes |
  
  ees,8[ f g aes]( bes4) f |
  g4 ees f2( |
  bes,1) \bar"||"
  ees2. bes4 |
  c ees aes,2 |
  ees'4 ees c2 |
  ees4 ees bes2 |
  ees ees |
  bes ees |
  
  %page2
  c4 aes2 ees'4 |
  f2 ees |
  r1 |
  r4 ees8[ f g aes]( bes4)~ |
  bes aes g4.( f8 |
  ees4) f g aes |
  bes4. bes8 f2 |
  
  r4 ees ees ees~ |
  ees f c4.( d8 |
  ees2) bes |
  ees1 \bar"||"
  ees2 bes |
  ees aes, |
  bes1 |
  ees2 r4 bes'~ |
  bes a bes2 |
  
  ees,4 f g aes~ |
  aes( g f2) |
  ees bes |
  ees c |
  bes c |
  bes( aes) |
  bes1 |
  ees \bar"|."
}
bassWords = \lyricmode {
  Red -- emp -- tó -- ris Ma -- ter, quæ pér -- vi -- a cæ -- li por -- ta ma -- nes,
  Et stel -- la ma -- ris, suc -- cúr -- re ca -- dén -- ti,
  súr -- ge -- re qui cu -- rat pó -- pu -- lo: __
  Tu quæ ge -- nu -- ís -- ti, na -- tú -- ra mi -- rán -- te, tu -- um sanc -- tum Ge -- ni -- tó -- rem:
  Vir -- go pri -- us ac post -- é -- ri -- us, Ga -- bri -- é -- lis ab o -- re
  su -- mens il -- lud A -- ve, pec -- ca -- tó -- rum mi -- se -- ré -- re,
    pec -- ca -- tó -- rum mi -- se -- ré -- re.
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
    \new ChoirStaff <<
    \new Staff = sop <<
      \new Voice = "sopranos" { \global \sopMusic }
    >>
    \new Lyrics = "sopranos"  \lyricsto "sopranos" \sopWords
    \new Staff = alt <<
      \new Voice = "altos" { \global \altoMusic }
    >>
    \new Lyrics = "altos"  \lyricsto "altos" \altoWords
    \new Staff = ten <<
      \clef "treble_8"
      \new Voice = "tenors" { \global \tenorMusic }
    >>
    \new Lyrics = "tenors" \lyricsto "tenors" \tenorWords
    \new Staff = bas <<
      \clef bass
      \new Voice = "basses" { \global \bassMusic }
    >>
    \new Lyrics = "basses" \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Lyrics
      \override LyricText.font-size = #1.3
      %\override VerticalAxisGroup.staff-affinity = #0
    }
    \context {
      \Score
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
      
      \override VerticalAxisGroup.staff-staff-spacing =
      #'((basic-distance . 2)
         (minimum-distance . 0)
         (padding . 2)
         (stretchability . 60))
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

