\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Adeste Fideles"}}
  poet = \markup\oldStyleNum"John Francis Wade (1711–1786)"
  composer = \markup\concat{"from " \italic"Cantus Diversi" \oldStyleNum", 1751"}
  tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -10)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #012
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
  \key a \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-0.5 . 1.5)
}

sopMusic = \relative c'' {
  \tempo 4 = 100
  \partial 4 a4 |
  a2 e4 a |
  b2 e, |
  cis'4 b cis d |
  \slurDotted cis4.( cis8) b4 a |
  a2 gis4( fis) | \break
  
  \slurSolid
  gis( a) \slurDotted b( cis) |
  \slurSolid gis2( fis4.) e8 |
  e2. b'4\rest |
  e2 d4 cis |
  \slurDotted d2( cis) |
  b4 cis a b |
  \slurSolid \partial 4*3 gis4.( fis8) e4 | \break
  
  \partial 4 a4 |
  a gis a b |
  a2 e4 cis'4 |
  cis b cis d |
  cis2 b4 \bar""\break cis |
  
  d cis b a |
  gis2 a4( d) |
  cis2( b4.) a8) |
  \partial 4*3 a2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \partial 4 e4 |
  e2 e4 e |
  e2 e |
  e4 e e fis |
  \slurDotted e4.( e8) e4 cis |
  \slurSolid cis( dis) \slurDotted e( dis) |
  
  \slurSolid e( dis8[) cis] \slurDotted b4( e) |
  \slurSolid e2( dis4.) e8 |
  e2. s4 |
  e2 fis8[ gis] a4 |
  \slurSolid a( \slurDotted gis)( a2) |
  e4 e fis fis |
  \slurSolid \partial 4*3 e2 e4 |
  
  e4 |
  e1~ |
  e2 e4 e |
  e e e e |
  e2 e4 a |
  
  gis a e e8[ dis] |
  e2 e4( fis) |
  e2( d4.) cis8 |
  \partial 4*3 cis2. \bar "|."
}
altoWords = {
  \dropLyricsV
  \lyricmode {
    \set stanza = #"1. "
  \set ignoreMelismata = ##t
    Ad -- e -- ste fi -- de -- les, Læ -- ti tri -- um -- phan -- _ tes,
    Ve -- ni -- te, ve -- ni -- _ te in Beth -- _ le -- hem; Na -- tum vi -- de -- te,
    Re -- gem an -- ge -- lo -- _ rum;
  \unset ignoreMelismata
  }
  \set stanza = \markup\dynamic"mf  "
  \lyricmode {
    Ve -- ni -- te ad -- o -- re -- mus,
  }
  \set stanza = \markup\dynamic" f "
  \lyricmode {
    Ve -- ni -- te ad -- o -- re -- mus,
  }
  \set stanza = \markup\dynamic"ff  "
  \lyricmode {
    \raiseLyrics
    Ve -- ni -- te ad -- o -- re -- mus, __ Do -- mi -- num.
  }
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  _ De -- um de De -- o, lu -- _ men de lu -- mi -- ne, ""
  Ges -- tant pu -- el -- _ læ __ _ vi -- _ sce -- ra.
  De -- um __ _ ve -- rum, ge -- ni -- tum non fac -- _ tum.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  _ Can -- tet nunc ‘I -- o,’ cho -- rus an -- ge -- lo -- _ rum_; ""
  Can -- tet nunc au -- _ la cæ -- lés -- _ ti -- um,
  Glo -- ri -- _ a __ _ in ex -- cél -- sis De -- _ o!
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  _ Er -- go qui na -- tus di -- e ho -- di -- er -- _ na. ""
  Je -- su, __ _ ti -- _ bi sit glo -- _ ri -- a,
  Pa -- tris æ -- ter -- ni Ver -- bum ca -- ro fac -- _ tum.
}

tenorMusic = \relative c' {
  \partial 4 cis4 |
  cis2 cis4 cis |
  b2 b |
  a4 b a a |
  \slurDotted a4.( a8) gis4 a |
  a2 b4( b) |
  
  \slurSolid b( a) \slurDotted e'( cis) |
  \slurSolid b2( a4.) gis8 |
  gis2. d4\rest |
  cis'2 d4 e |
  \slurDotted e2( e2) |
  e4 a, cis d |
  \slurSolid \partial 4*3 b4.( a8) gis4 |
  
  \partial 4 cis4 |
  cis b cis d |
  cis2. a4 |
  a gis a b |
  a2 gis4 e' |
  
  e e b b |
  b2 a |
  a( gis4.) a8 |
  \partial 4*3 a2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  \partial 4 a4 |
  a2 a4 a |
  gis2 gis |
  a4 gis a d, |
  \slurDotted e4.( e8) e4 fis |
  fis2 e4( b) |
  
  \slurSolid e( cis) \slurDotted gis( a) |
  b2~ b4. e8 |
  e2. s4 |
  cis'2 b4 a |
  b2( a2) |
  gis4 a fis d |
  \partial 4*3 e2 e4 |
  
  \slurSolid \partial 4 d\rest |
  g,1\rest |
  g1\rest |
  g1\rest |
  g2\rest f4\rest a'4 |
  
  b a gis fis |
  e( d) cis( d) |
  e2~e4. a,8 |
  \partial 4*3 a2. \bar "|."
}
bassWords = \lyricmode {

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
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
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
    \tempo 4 = 100
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
