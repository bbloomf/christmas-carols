\version "2.24.0"
\include "util.ly"
\header {
    tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
}
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
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
  first-page-number = #198
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner.style = #'none
}

sopMusic = \relative c' {
	a'1 |
  d,2 a'~ |
  a4 a bes bes |
  a2 r4 d |
  bes c d4. d8 |
  
  d4 a bes a(~ |
  a8[ g f e] f[ g a f] |
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
  O ma -- gnum mys -- té -- ri -- um et ad -- mi -- rá -- bi -- le sa -- cra -- mén -- tum.
    O ma -- gnum mys -- té -- ri -- um et ad -- mi -- rá -- bi -- le,
      et ad -- mi -- rá -- bi -- le
    sa -- cra -- mén -- tum,
  ut a -- ni -- má -- li -- a vi -- dé -- rent Dó -- mi -- num na -- tum,
    vi -- dé -- rent Dó -- mi -- num na -- tum
  ja -- cén -- tem in præ -- sé -- pi -- o,
    ja -- cén -- tem in præ -- sé -- pi -- o.
  
  O be -- á -- ta vir -- go cu -- jus ví -- sce -- ra
  me -- ru -- é -- runt por -- tá -- re Dó -- mi -- num Je -- sum Chris -- tum.
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja. __
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
  bes4.( c8 d4) ees(~ |
  ees d c4. d8 |
  ees2) d~ |
  d1 \bar"|."
}
altoWords = \lyricmode {
  O ma -- gnum mys -- té -- ri -- um et ad -- mi -- rá -- bi -- le sa -- cra -- mén -- tum.
    O ma -- gnum mys -- té -- ri -- um et ad -- mi -- rá -- bi -- le sa -- cra -- mén -- tum,
      et ad -- mi -- rá -- bi -- le sa -- cra -- mén -- tum,
  ut a -- ni -- má -- li -- a vi -- dé -- rent Dó -- mi -- num na -- tum,
    vi -- dé -- rent Dó -- mi -- num na -- tum
  ja -- cén -- tem,
    ja -- cén -- tem
  in præ -- sé -- pi -- o,
    ja -- cén -- tem in præ -- sé -- pi -- o.
  
  O be -- á -- ta vir -- go cu -- jus ví -- sce -- ra
  me -- ru -- é -- runt por -- tá -- re Dó -- mi -- num Je -- sum Chris -- tum.
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja. __
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
  O ma -- gnum mys -- té -- ri -- um et ad -- mi -- rá -- bi -- le sa -- cra -- mén -- tum,
    et ad -- mi -- rá -- bi -- le sa -- cra -- mén -- tum,
  ut a -- ni -- má -- li -- a,
    ut a -- ni -- má -- li -- a
  vi -- dé -- rent Dó -- mi -- num na -- tum,
    vi -- dé -- rent Dó -- mi -- num na -- tum
  ja -- cén -- tem in præ -- sé -- pi -- o,
    ja -- cén -- tem in præ -- sé -- pi -- o,
      in præ -- sé -- pi -- o.
  
  O be -- á -- ta vir -- go cu -- jus ví -- sce -- ra
  me -- ru -- é -- runt por -- tá -- re Dó -- mi -- num Je -- sum Chris -- tum.
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja.
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
  O ma -- gnum mys -- té -- ri -- um et ad -- mi -- rá -- bi -- le sa -- cra -- mén -- tum,
    et ad -- mi -- rá -- bi -- le sa -- cra -- mén -- tum,
  ut a -- ni -- má -- li -- a vi -- dé -- rent Dó -- mi -- num na -- tum,
  ja -- cén -- tem in præ -- sé -- pi -- o,
    ja -- cén -- tem in præ -- sé -- pi -- o,
    in præ -- sé -- pi -- o.
  
  O be -- á -- ta vir -- go cu -- jus ví -- sce -- ra
  me -- ru -- é -- runt Je -- sum Chris -- tum.
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja,
  Al -- le -- lú -- ja. __
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
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 65))
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"O Magnum Mysterium"}}
    composer = \markup\oldStyleNum"Tomás Luis de Victoria (c. 1548–1611)"
    tagline = ""
  }
}






























global = {
  \key d \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  e4 e |
  b'2 a4 a b2 b4 b |
  e2 cis4 d |
  b2 a4 b |
  
  d2 a4 b |
  g2 |
  fis4 e fis d |
  e2 e |
  fis4 g a d, |
  
  e2 e |
  fis4 g |
  a2 a |
  a g4 a |
  b2 b |
  b fis4 g |
  a2 g4 fis |
  e2 |
  fis4 e e d |
  e2 e\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  b4 b |
  d2 d4 d |
  d2 d4 e |
  g2 fis4 fis |
  fis2 fis4 g |
  
  fis2 fis4 fis |
  e2 |
  d4 cis cis d |
  cis2 b |
  d4 e e d |
  
  b2 b |
  d4 d |
  fis2 e |
  fis g4 fis |
  g2 fis |
  
  g fis4 d |
  d2 e4 dis |
  e2 |
  d?4 b a a |
  cis2 b \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Pér -- so -- nent hó -- di -- e
  vo -- ces pu -- é -- ru -- læ,
  lau -- dán -- tes ju -- cún -- de
  Qui no -- bis est na -- tus,
  sum -- mo De -- o da -- tus,
  et de vir-, vir-, vir-,
  et de vir-, vir-, vir-,
  et de vir -- gí -- ne -- o
  ven -- tre pro -- cre -- á -- tus.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  In mun -- do ná -- sci -- tur,
  pan -- nis in -- vól -- vi -- tur,
  præ -- sé -- pi pó -- ni -- tur
  stá -- bu -- lo bru -- tó -- rum,
  rec -- tor su -- per -- nó -- rum,
  pér -- di -- dit, -dit, -dit,
  pér -- di -- dit, -dit, -dit,
  pér -- di -- dit spó -- li -- a
  prin -- ceps in -- fer -- nó -- rum.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Ma -- gi tres ve -- né -- runt,
  pár -- vu -- lum in -- quí -- runt,
  Béth -- le -- hem ád -- e -- unt,
  stél -- lu -- lam se -- quén -- do,
  ip -- sum ad -- o -- rán -- do,
  au -- rum thus, thus, thus,
  au -- rum thus, thus, thus,
  au -- rum thus, et myr -- rham
  E -- i of -- fe -- rén -- do.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  Om -- nes cle -- rí -- cu -- li,
  pár -- i -- ter pú -- e -- ri,
  can -- tent ut án -- ge -- li:
  Ad -- ve -- nís -- ti mun -- do,
  lau -- des Ti -- bi fun -- do.
  Id -- e -- o, -o, -o,
  íd -- e -- o, -o, -o,
  íd -- e -- o, gló -- ri -- a
  in ex -- cél -- sis De -- o.
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  g4 g |
  fis2 fis4 fis |
  g2 g4 b |
  b2 ais4 b |
  d2 d4 d |
  
  d2 d4 d |
  c4( b) |
  a4 a a fis |
  a2 g |
  b4 b a a |
  
  a( fis) gis2 |
  a4 b |
  d2 d4( cis) |
  d2 d4 c |
  e2 e4( dis) |
  
  e2 d4 b |
  a2 b4 a |
  b2 |
  a4 g e fis |
  a2 gis \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  e4 e |
  b2 d4 d |
  g,2 g'4 g |
  e2 fis4 b, |
  b2 d4 g |
  
  d2 d4 b |
  c2 |
  d4  a fis b |
  a2 e' |
  b4 e cis fis |
  
  e2 e |
  d4 g |
  fis( g) a2 |
  d, b'4 a |
  g( a) b2 |
  
  e, b'4 g |
  fis2 e4 fis |
  g2 |
  d4 e cis d |
  a2 e'\fermata \bar "|."
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
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Personent hodie"}}
    poet = \markup \concat{"from " \italic "Piæ Cantiones" \oldStyleNum", 1582"}
    composer = \markup \concat{"from " \italic "Piæ Cantiones" \oldStyleNum", 1582"}
    arranger = \markup\oldStyleNum"Arranged by George Ratcliffe Woodward (1848–1934)"
    tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
  }
}
