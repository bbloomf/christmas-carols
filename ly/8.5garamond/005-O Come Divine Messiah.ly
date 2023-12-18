\version "2.24.0"
\include "util.ly"
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"O Come, Divine Messiah"}}
  poet = \markup\oldStyleNum"Abbé Simon J. Pellegrin (1663–1745)"
  meter = \markup\oldStyleNum"Translated by Sister Mary of St. Philip, SND (1825–1904)"
  composer = \markup\oldStyleNum"16th Century French Carol"
  %arranger = \markup\oldStyleNum"Edited by Benjamin Bloomfield (1984–)"
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
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #005
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCaps advent}
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

sopMusic = \relative c' {
  \partial 8 d8 |
  g4 a8 b4 b8 |
  c4. b4 a8 |
  g4 fis8 e4 a8 |
  g[ fis] e d4 d8 |
  g4 a8 b4 b8 |
  
  c4. b4 d8 |
  b4 b8 a[ g] a |
  g4.~ g4 b8\rest | \break
  a4. g4 fis8 |
  g4.~ g4. |
  a4. g4 fis8 |
  
  g4 g8 c4 b8 |
  a4 d8 c4 b8 |
  a4 d8 c4 b8 |
  a4 g8 fis4 g8 |
  a4.~ a4 d,8 |
  g4 a8 b4 b8 |
  
  c4. b4 a8 |
  g4 fis8 e4 a8 |
  g[ fis] e d4 d8 |
  d'4 d8 b4 g8 |
  
  c4. b4 d8 |
  b4 b8 a[ g] a |
  g2.^\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 |
  b4 c8 d4 d8 |
  e4. d4 d8 |
  d4 d8 c4 e8 |
  d4 cis8 d4 d8 |
  d4 c8 d4 d8 |
  
  e4. e4 d8 |
  d4 d8 d4 d8 |
  c( a4 b4) s8 |
  e4. d4 a8 |
  b4.~ b4. |
  e4. d4 a8 |
  
  e'4 e8 e[ fis] g |
  fis[ e] fis e4 g8 |
  fis4 fis8 g4 g8 |
  e4 e8 e4 e8 |
  d4.~ d4 d8 |
  b4 c8 d4 d8 |
  
  e4. d4 d8 |
  d4 d8 c4 e8 |
  d4 cis8 d4 d8 |
  d[ e] fis g4 d8 |
  
  e4( fis8) g4 fis8 |
  d4 d8 d4 c8 |
  d8( c4 b4.)\fermata \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsVII
  \set stanza = #"1. "
  O come, di -- vine Mes -- si -- ah!
  The world in si -- lence waits the day
  When hope shall sing its tri -- umph,
  And sad -- ness flee a -- way. __
  
  \dropLyricsV
  Dear Sav -- ior haste;
  Come, come to earth,
  Dis -- pel the night and show Your face,
  And bid us hail the dawn of grace.

  O come, di -- vine Mes -- si -- ah!
  The world in si -- lence
  \break
  \dropLyricsVII
  waits the day
  When hope shall sing its tri -- umph,
  And sad -- ness flee a -- way. __
}
altoWordsII = \lyricmode {
  \dropLyricsVII
%\markup\italic
  \set stanza = #"2. "
  O Christ, whom na -- tions sigh for,
  Whom priest and pro -- phet long fore -- told,
  Come break the cap -- tive fet -- ters;
  Re -- deem the long -- lost fold. __
}
altoWordsIII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"3. "
  You come in peace and meek -- ness,
  And low -- ly will Your cra -- dle be;
  All clothed in hu -- man weak -- ness
  Shall we Your God -- head see. __
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c {
  d8 |
  d4 g8 b4 b8 |
  g4. g4 fis8 |
  b4 b8 c[ b] a |
  a4 g8 fis[ g] a |
  b4 g8 b4 b8 |
  
  g4. g4 a8 |
  b4 g8 fis4 fis8 |
  g4.~ g4 s8 |
  a4. a4 d8 |
  d4.~ d4. |
  a8[ b c] b4 d8 |
  
  b4 b8 c4 b8 |
  d4 a8 c4 b8 |
  d4 d8 e4 d8 |
  c4 c8 a4 a8 |
  fis4.~ fis4 d8 |
  d4 g8 b4 b8 |
  
  g4. g4 fis8 |
  b4 b8 c[ b] a |
  a4 g8 fis[ g] a |
  fis[ g] a d4 b8 |
  
  g4( fis8) e4 a8 |
  g4 g8 fis[ g] fis |
  b([ a8 fis] g4.)^\fermata \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8 |
  g,4 g8 g4 g8 |
  c4. g4 a8 |
  b4 b8 c4 cis8 |
  d4 a8 d[ e] fis |
  g4 g8 g4 g,8 |
  
  c4( d8) e4 fis8 |
  g4 b,8 d4 d8 |
  g,4.~ g4 d'8\rest |
  c4( a8) d4 d8 |
  g,4.~ g4. |
  c8[ b a] b4 d8 |
  
  e4 e8 c[ d] e |
  d[ e] d c[ d] e |
  d[ c] b a4 b8 |
  c4 c8 cis4 cis8 |
  d4.~ d4 d8 |
  g,4 g8 g4 g8 |
  
  c4. g4 a8 |
  b4 b8 c4 cis8 |
  d4 a8 d4 fis8 |
  b,4 b8 g4 g'8 |
  
  c,4. e4 d8 |
  g,8[ a] b d8[ e] d |
  g,2._\fermata \bar "|."
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWords
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
}

