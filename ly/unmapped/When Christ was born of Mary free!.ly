\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"When Christ was born of Mary free!"}}
  poet = \markup\oldStyleNum"15th Century Middle English Harleian Manuscript"
  composer = \markup\oldStyleNum"16th Century English Tune"
  arranger = \markup\oldStyleNum"Arranged by Sir John Stainer (1840–1901)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #017
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
  \key g \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(1 . 1)
  \override DynamicText #'X-offset = #-3.5
}

sopMusic = \relative c'' {
  \set Staff.midiInstrument = "acoustic grand"
  \partial 4 g4 |
  d' b g a8[ b] |
  c4 b8[ a] b4 a8[ g] |
  fis[ e] <e' c>[ <d b>] <c a>[ <b g>] a[ g] |
  \partial 4*3 g4. fis8 g4 | \break
  \set Staff.midiInstrument = "piccolo"
  
  \mark \markup {\musicglyph #"scripts.segno"} \partial 4 g4_\f |
  \slurDotted d'8\noBeam(d) \slurSolid b4 g a8[ b] |
  c4 b8[ a] b4 b |
  a4. a8 b4 d |
  
  e d8[ cis] d2 |
  a4 a8\noBeam b8 c4 c |
  g4. a8 b2 |
  d4._\p e8 d4 c8[ b] |
  
  a4. g8 g4 b4\rest | \bar "||" \break
  d,4. d8 d4 d |
  e g g2 |
  a4. b8 c4 c |
  
  b a b2 | \break
  d4. d8 g,8[ a] b[ c] |
  d4 c8[ b] a2 |
  b4. c8 d4 g, |
  \partial 4*3 b a g4\fermata \bar "|."
  
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \set Staff.midiInstrument = "acoustic grand"
  d4 |
  d fis e e |
  e <d fis> <d g> d |
  d8[ e] e4 e8[ d] c4 |
  <b d> <a d> <b d> |
  
  \set Staff.midiInstrument = "piccolo"
  d |
  \slurDotted d8\noBeam( d) \slurSolid fis4 g e |
  a fis g g |
  d4. d8 g4 fis |
  
  g e fis2 |
  fis4 f8\noBeam f a4 a |
  e ees g2 |
  g4. g8 g4 fis8[ g] |
  
  g4 fis g s | \bar "||"
  d4. d8 d4 d |
  e g g2 |
  fis4. fis8 g4 g |
  
  g fis g2 |
  a4 g8[ fis] g[ d] g4 |
  d8[ g] a[ g] fis2 |
  d8[ g] fis[ e] d4 g |
  
  g fis g \bar "|."
}
altoWords = {
  \dropLyricsVI
  \lyricmode {
    \repeat unfold 16 { \skip 1 }
    \set stanza = #"1. "
    When Christ was born of Ma -- ry free,
    In Beth -- le -- hem, that fair ci -- ty,
    An -- gels sang there with mirth and glee,
    \markup\italic “In \markup\italic ex -- \markup\italic cel -- \markup\italic sis \markup\italic Glo -- \markup\italic ri -- \markup\italic a.”
  }
  \set stanza = \markup\dynamic"ff "
  \lyricmode {
    \dropLyricsIV
    \markup\italic In \markup\italic ex -- \markup\italic cel -- \markup\italic sis \markup\italic Glo -- \markup\italic ri -- \markup\italic a,
    \markup\italic In \markup\italic ex -- \markup\italic cel -- \markup\italic sis \markup\italic Glo -- \markup\italic ri -- \markup\italic a,
    \markup\italic In \markup\italic ex -- \markup\italic cel -- \markup\italic sis \markup\italic Glo -- \markup\italic ri -- \markup\italic a,
    \markup\italic In \markup\italic ex -- \markup\italic cel -- \markup\italic sis \markup\italic Glo -- \markup\italic ri -- \markup\italic a.
  }
}
altoWordsII = \lyricmode {
  \dropLyricsVI
  \repeat unfold 16 { \skip 1 }
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  _ Herds -- men
  \unset ignoreMelismata
  be -- held these An -- gels bright,
  To them ap -- pear -- ing with great light,
  Who said God’s Son is born to -- night,
  \markup\italic “In \markup\italic ex -- \markup\italic cel -- \markup\italic sis \markup\italic Glo -- \markup\italic ri -- \markup\italic a.”
}
altoWordsIII = \lyricmode {
  \dropLyricsVI
  \repeat unfold 16 { \skip 1 }
  \set stanza = #"3. "
  The King is come to save man -- kind, ""
  As in scrip -- ture truths we find,
  There -- fore this song we have in mind,
  \markup\italic “In \markup\italic ex -- \markup\italic cel -- \markup\italic sis \markup\italic Glo -- \markup\italic ri -- \markup\italic a.”
}
altoWordsIV = \lyricmode {
  \dropLyricsVI
  \repeat unfold 16 { \skip 1 }
  \set stanza = #"4. "
  _ Then dear Lord, for Thy great grace,
  Grant us in bliss to see Thy face,
  That we may sing to Thy sol -- ace,
  \markup\italic “In \markup\italic ex -- \markup\italic cel -- \markup\italic sis \markup\italic Glo -- \markup\italic ri -- \markup\italic a.”
}

tenorMusic = \relative c' {
  \set Staff.midiInstrument = "acoustic grand"
  b4 |
  a d b c, |
  a d g g |
  g a,8[ b] c[ d] e4 |
  d d, g \bar "||"
  
  \set Staff.midiInstrument = "piccolo"
  b'^\f |
  \slurDotted a8\noBeam( a) \slurSolid d4 b e |
  e d d d |
  d4. d8 d4 d |
  
  b a a2 |
  d4 d8\noBeam d e4 e |
  c c d2
  d4.^\p c8 d4 d |
  
  d c b s \bar "||"
  d,4. d8 d4 d |
  e g g2 |
  d'4. d8 e4 e |
  d d d2 |
  fis8[ e] d[ c] b[ a] g[ a] |
  b4 e8[ d] d2 |
  d4. c8 b4 cis |
  
  d c b \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  \set Staff.midiInstrument = "acoustic grand"
  g4 |
  fis d e s |
  s4*3 b4 |
  c s4*3 |
  s4*3 \bar "||"
  
  \set Staff.midiInstrument = "piccolo"
  g'4 |
  \slurDotted fis8\noBeam( fis) \slurSolid d4 e c |
  a d g g |
  fis4. fis8 g4 b |
  
  e, a d,2 |
  d4 d8\noBeam d a4. b8 |
  c4 c g'2 |
  b4. c8 b4 a8[ g] |
  
  d4. d8 g,4 d'\rest |
  d4. d8 d4 d |
  e g g2 |
  d4. d8 c4 c |
  
  g' d g2 |
  d'8[ c] b[ a] g[ fis] e4 |
  b c d2 |
  g,4. a8 b4 e |
  
  d d g,\fermata \bar "|."
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
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWords
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
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
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
    \tempo 4 = 105
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
