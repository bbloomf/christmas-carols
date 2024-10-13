\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"O Magnum Mysterium"}}
  composer = \markup\oldStyleNum"Tomás Luis de Victoria (c. 1548–1611)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
%  system-system-spacing =
%    #'((basic-distance . 0)
%       (minimum-distance . 0)
%       (padding . -3)
%       (stretchability . 100))
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
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	a'1 |
  d,2 a'~ |
  a4 a bes bes |
  a2 r4 d |
  bes c d4. d8 |
  
  d4 a bes a(~ |
  a8[ g f e f g a f] |
  g[ f] f[ e16 d] e2~ |
  e) d |
  fis1 |
  
  %page2
  g2 fis4.( g8 |
  a4) bes4(~ bes8[ a] g4~ |
  g fis) g( f8[ e] |
  d4) e f2 |
  r4 d' bes c |
  
  d4. d8 d4 bes |
  g a bes4. bes8 |
  bes4 a4. g8 g4(~ |
  g fis) g2 |
  r1 |
  
  r4 bes a4. f8 |
  g4 a bes g |
  bes4. bes8 bes4 bes |
  a2 a |
  fis4 a a4. a8 |
  
  %page3
  a4 b c2 |
  bes?4.( a16[ g] a2 |
  bes) a |
  r1 |
  r4 d, g2 |
  
  f4 d e( fis) |
  g4.( a8 bes4) bes |
  a2 r |
  r1 |
  r4 g c2 |
  
  bes4 g a( b) |
  c4.( bes?8[ a g] g4~ |
  g fis8[ e] fis4) fis |
  g2 r \bar"||"\pageBreak
  fis2. fis4 |
  fis2 g |
  
  %page4
  fis4.( g8 a4 bes~ |
  bes8[[ a] a4. g8 g4) |
  a fis2 fis4 |
  g4. g8 g2 |
  r4 g4~( g8[ a bes g] |
  
  a4) d c2 |
  bes4 d c a |
  bes4.( a8[ g f] f[ e16 d] |
  e4) e d a'~ |
  a f2 bes4~( |
  
  bes8[ a] g2 fis4) |
  \time 3/4
  g2 bes4 |
  a2 fis4 |
  g4. f?8[ g a] |
  bes4 a2 |
  bes2 g4 |
  
  %page5
  f2 d4 |
  e4. d8[ e f] |
  g2 fis4 |
  g2. |
  r2 c4 |
  bes4.( a8[ bes g] |
  
  a4) f8[ g a bes] |
  c2 c4 |
  bes4 d2 |
  \time 4/4
  d1 |
  r4 d d8([ c bes a] |
  
  g4) c4(~ c8[ bes a g] |
  fis4 g2 fis4) |
  g1~ |
  g~ |
  g~ |
  g \bar"|."
}
sopWords = \lyricmode {
  O ma -- gnum my -- ste -- ri -- um et ad -- mi -- ra -- bi -- le sa -- cra -- men -- tum.
    O ma -- gnum my -- ste -- ri -- um et ad -- mi -- ra -- bi -- le,
      et ad -- mi -- ra -- bi -- le
    sa -- cra -- men -- tum,
  ut a -- ni -- ma -- li -- a vi -- de -- rent Do -- mi -- num na -- tum,
    vi -- de -- rent Do -- mi -- num na -- tum
  ja -- cen -- tem in præ -- se -- pi -- o,
    ja -- cen -- tem in præ -- se -- pi -- o.
  
  O be -- a -- ta vir -- go cu -- jus vi -- sce -- ra
  me -- ru -- e -- runt por -- ta -- re Do -- mi -- num Je -- sum Chri -- stum.
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja. __
}

altoMusic = \relative c' {
  r1 |
  r2 d~ |
  d g, |
  d'2. d4 |
  ees4 ees d2 |
  
  r4 d bes c |
  d4. d8 d4 a |
  bes d2( cis8[ b] |
  cis2) d |
  d1 |
  
  %page2
  d2 d4.( e8 |
  f4) f, g4.( a8 |
  bes4) a g d' |
  d c a4. a8 |
  a4 bes4. a8 g4(~ |
  g fis) g g' |
  d f f4. f8 |
  f2 d4 ees |
  d2 d~ |
  d r2 |
  
  r4 g f4. d8 |
  e4 fis g d |
  d4. d8 d4 e |
  f2 e |
  d4 f f4. f8 |
  
  %page3
  f4 f g a(~ |
  a8[ g] g2 fis4) |
  g d f4.( e16[ f] |
  g4 f8[ e] d4) a |
  r2 r4 g |
  
  d'2 c4 a |
  b4( c) d8([ c d e] |
  f4. e16[ f] g4) f8[ e] |
  d2 r4 a |
  d2 c4 a |
  
  bes8([ c d e] f2) |
  g4.( f8[ ees d] ees4 |
  d2.) d4 |
  b2 r2 \bar"||"
  d2. d4 d2 d |
  
  %page 4
  d1~ |
  d |
  d2 r4 d~ |
  d d ees4. ees8 |
  d4 g,8([ a] bes[ c d e] |
  f4) f f2 |
  d4 d e fis |
  g4.( f?8[ e d] d4~ |
  d) cis d f~( |
  f8[ e d c] d2 |
  
  bes4) c d2 |
  \time 3/4
  d2 g4 |
  f2 d4 |
  e4. d8[ e f] |
  g2 fis4 |
  g2 d4 |
  
  %page 5
  d2 a4 |
  c2 c4 |
  d d2 |
  d bes4 |
  g2 fis4 |
  g2 g'4 |
  
  f2 d4 |
  e4. d8[ e f] |
  g2 fis4 |
  \time 4/4
  g2 r4 d |
  d8([ c bes a] g4) g'~( |
  g8[ f ees d] c4 ees) |
  d2 r4 a |
  bes4.( c8 d4) ees~( |
  ees d c4. d8 |
  ees2) d~ |
  d1 \bar"|."
}
altoWords = \lyricmode {
  O ma -- gnum my -- ste -- ri -- um et ad -- mi -- ra -- bi -- le sa -- cra -- men -- tum.
    O ma -- gnum my -- ste -- ri -- um et ad -- mi -- ra -- bi -- le sa -- cra -- men -- tum,
      et ad -- mi -- ra -- bi -- le sa -- cra -- men -- tum,
  ut a -- ni -- ma -- li -- a vi -- de -- rent Do -- mi -- num na -- tum,
    vi -- de -- rent Do -- mi -- num na -- tum
  ja -- cen -- tem,
    ja -- cen -- tem
  in præ -- se -- pi -- o,
    ja -- cen -- tem in præ -- se -- pi -- o.
  
  O be -- a -- ta vir -- go cu -- jus vi -- sce -- ra
  me -- ru -- e -- runt por -- ta -- re Do -- mi -- num Je -- sum Chri -- stum.
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja. __
}

tenorMusic = \relative c' {
  \repeat unfold 7 { r1 }
  r2 a2~ |
  a d, |
  a'2. a4 |
  
  %page2
  bes4 bes a2 |
  r4 d bes c |
  d4. d8 d4 a |
  bes a~( a8[ g f e] |
  f2 g |
  
  a) bes4 d |
  bes c d4. d8 |
  d4 c bes( c) |
  a2 g4 bes |
  a4. f8 g4 a |
  
  bes g d'4. d8 |
  c4 a g bes |
  bes4. bes8 bes4 bes |
  c d2( cis4) |
  d d, d'4. d8 |
  
  %page3
  d4 d c2 |
  ees d |
  r4 g, d'2 |
  c4 a b( c) |
  d8([ c bes? a] g[ a bes g] |
  a2.) a4 |
  g2 r4 g |
  d'2 c4 a |
  bes a4~( a8[ g f e] |
  f4 g2) fis4 |
  
  g bes c( d) |
  ees4.( d8[ c bes] c4 |
  bes a8[ g] a4) a |
  g2 r \bar"||"
  a2. a4 a2 bes |
  
  %page4
  a2.( g4 a2 bes) |
  a4 a2 a4 |
  b4. b8 c4 g8([ a] |
  bes?[ c] d2) bes4 |
  
  c4( bes2 a4) |
  bes bes c d |
  g,2. bes4 |
  a1 f |
  
  g2 a |
  \time 3/4
  g2 g4 |
  d'2 d4 |
  c2 c4 |
  g d'2 |
  g, bes4 |
  
  %page5
  a2 f4 |
  g4. f8[ g a] |
  bes4 a2 |
  g d4 |
  e c2 |
  d bes'4 |
  
  a2 f4 |
  g2 g4 |
  g a2 |
  \time 4/4
  g4 bes a8([ g f e] |
  d4) g8([ a] bes[ c d bes] |
  c4. d8 ees[ d c bes] |
  a4 g) a d |
  d8([ c bes a] g4) bes( |
  c d ees4. d8 |
  c2. b8[ a]) |
  b1 \bar"|."
}

tenorWords = \lyricmode {
  O ma -- gnum my -- ste -- ri -- um et ad -- mi -- ra -- bi -- le sa -- cra -- men -- tum,
    et ad -- mi -- ra -- bi -- le sa -- cra -- men -- tum,
  ut a -- ni -- ma -- li -- a,
    ut a -- ni -- ma -- li -- a
  vi -- de -- rent Do -- mi -- num na -- tum,
    vi -- de -- rent Do -- mi -- num na -- tum
  ja -- cen -- tem in præ -- se -- pi -- o,
    ja -- cen -- tem in præ -- se -- pi -- o,
      in præ -- se -- pi -- o.
  
  O be -- a -- ta vir -- go cu -- jus vi -- sce -- ra
  me -- ru -- e -- runt por -- ta -- re Do -- mi -- num Je -- sum Chri -- stum.
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja.
}

bassMusic = \relative c' {
  \repeat unfold 9 { r1 }
  d,1 |
  
  %page2
  g,2 d'~ |
  d4 d ees ees |
  d2 r4 d |
  bes c d4. d8 |
  d4 bes ees2 |
  
  d2 g,4 g |
  g' f bes,4. bes8 |
  bes4 f' g( c,) |
  d2 g,4 g' |
  f4. d8 e4 fis |
  
  g2 r |
  r r4 g, |
  g'4. g8 g4 g |
  f4.( g8) a2 |
  d,2 r |
  
  %page3
  r1 |
  r2 r4 d |
  g2 f4 d |
  e( f) g( f8[ e] |
  d4. f8 ees4) ees |
  
  d2 r |
  r4 c g'2 |
  f4 d e( f) |
  g( f8[ e] d4. c8 |
  bes4 a8[ g] a4) a |
  
  g g' f( d) |
  c1( |
  d2.) d4 |
  g,2 r \bar"||"
  d'2. d4 |
  d2 g, |
  
  %page4
  d'4.( e8 fis4 g~ |
  g8[ fis] fis4 g2) |
  d4 d2 d4 |
  g4. g8 c,2 |
  g'2. g4 |
  
  f4( bes, f'2) |
  bes, r |
  r1 |
  r2 d~ |
  d bes |
  
  ees( d) |
  \time 3/4
  g,2 r4 |
  \repeat unfold 3 { r2. }
  r2 g4 |
  
  %page5
  d'2 d4 |
  c2 c4 |
  g d'2 |
  g, g4 |
  c a2 |
  g g4 |
  
  d'2 d4 |
  c2 c4 |
  ees d2 |
  \time 4/4
  g,4 g' f8([ e d c] |
  bes[ a g f] g2) |
  
  c1( |
  d) |
  g,4 g' g8([ f ees d] |
  c4 b) c2~ |
  c g~ |
  g1 \bar"|."
}
bassWords = \lyricmode {
  O ma -- gnum my -- ste -- ri -- um et ad -- mi -- ra -- bi -- le sa -- cra -- men -- tum,
    et ad -- mi -- ra -- bi -- le sa -- cra -- men -- tum,
  ut a -- ni -- ma -- li -- a vi -- de -- rent Do -- mi -- num na -- tum,
  ja -- cen -- tem in præ -- se -- pi -- o,
    ja -- cen -- tem in præ -- se -- pi -- o,
    in præ -- se -- pi -- o.
  
  O be -- a -- ta vir -- go cu -- jus vi -- sce -- ra
  me -- ru -- e -- runt Je -- sum Chri -- stum.
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja,
  Al -- le -- lu -- ja. __
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
      \clef "treble_8"
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
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
%      \override VerticalAxisGroup #'staff-staff-spacing =
%      #'((basic-distance . 0)
%         (minimum-distance . 0)
%         (padding . -1)
%         (stretchability . 2))
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  \midi {
    \tempo 4 = 100
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

