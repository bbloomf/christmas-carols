\version "2.24.0"
\include "util.ly"
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 35)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 200))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0.5)
       (stretchability . 0))
  system-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0.5)
       (stretchability . 0))
  
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #055
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
  \key d \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 4 fis8.[ g16] |
  a4 d cis b |
  a d,8.[ e16] fis4 fis8.[ g16] |
  a4 a a g8[ fis] |
  
  \partial 4*3 fis4( e) b'\rest \bar "||"
  \partial 4 cis8[ d] | \break
  e4 a, \bar"" g g |
  g fis8[ e] fis4 d'8[ cis] |
  
  b4 a g fis |
  \partial 4*3 b( a) b\rest \bar "||"
  \partial 4 e |
  a, d fis, e |
  \partial 4*3 d2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 |
  d4 fis e e |
  fis d8.[ cis16] d4 d |
  d fis d e8[ d] |
  
  d4( cis) s |
  e |
  e fis d cis |
  e d d d |
  
  d d d d |
  d2 s4 |
  g |
  a fis d4 cis |
  d2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsXI
  \set stanza = #"1. "
  While shep -- herds watch’d their flocks by night;
    All seat -- ed on the ground;
  The an -- gel of the Lord came down,
    And glo -- ry shone a -- round,
    And glo -- ry shone a -- round.
}
altoWordsII = \lyricmode {
  \dropLyricsXI
  \set stanza = #"2. "
  “To you, in Da -- vid’s town, this day
    Is born of Da -- vid’s line,
  A Sav -- ior, Who is Christ the __ Lord,
    And this shall be the sign,
    And this shall be the sign:
}
altoWordsIII = \lyricmode {
  \dropLyricsXI
  \set stanza = #"3. "
  The heav’n -- ly Babe you there shall find,
    To hu -- man view dis -- play’d,
  All mean -- ly wrapp’d in swad -- dling bands,
    And in a man -- ger laid,
    And in a man -- ger laid.”
}
altoWordsIV = \lyricmode {
  \dropLyricsXI
  \set stanza = #"4. "
  “All glo -- ry be to God on high,
    And to the earth be peace;
  Good -- will hence -- forth from heav’n to __ men
    Be -- gin, and nev -- er cease!
    Be -- gin, and nev -- er cease!”
}

tenorMusic = \relative c' {
  a4 |
  a a a b8[ cis] |
  d4 a a a |
  a a a b |
  
  a2 s4 |
  a |
  a fis b a |
  a a8[ g] a4 fis8[ a] |
  
  g4 fis b a |
  g( fis) s |
  b8[ cis] |
  d4 a a g |
  fis2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8.[ e16] |
  fis4 d a' g |
  fis fis8.[ e16] d4 d8.[ e16] |
  fis4 d fis g |
  
  a2 d,4\rest |
  a8[ b] |
  cis4 d e a, |
  d d d d |
  
  d d d d |
  d2 d4\rest |
  g4 |
  fis d a'4 a, |
  d2. \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.9)) } \lyricsto "sopranos" \altoWords
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
    %#(layout-set-staff-size 14.6)
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
    \context {
      \Score
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/4)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
%6.14g \context { \Lyrics \override LyricText.font-size = #0.6 }
%{IF_LESSER
\context {
  \Lyrics
  \override LyricText.font-size = #1.1
}
%}%END_IF_LESSER
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"While Shepherds Watched Their Flocks"}}
    poet = \markup\oldStyleNum"Nahum Tate (1652–1715)"
    composer = \markup\oldStyleNum"Adapted from George F. Handel"
  }
}
\markup \fill-line \center-align { \concat { "from " \italic\oldStyleNum "Sunday School Hymns No. 1" \oldStyleNum", 1903, via " \italic"HymnsAndCarolsOfChristmas.com"}}
\markup\vspace#0.5














\header {
  tagline = \markup\concat { "from " \italic "Church Sunday School Hymn-Book" \oldStyleNum", 1892, via " \italic"HymnsAndCarolsOfChristmas.com"}
}
global = {
  \key g \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \tempo 4 = 92
  \partial 4 g'4 |
  b4. b8 a4 g |
  c c b \bar"||"
  a |
  b d d cis |
  d2. \bar "||"
  
  b4 e4. d8 c4 b |
  a g fis \bar"||"
  b |
  a g g fis |
  g2. \bar"|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 |
  g4. g8 fis4 e |
  e g g |
  fis |
  g a a a |
  fis2. |
  
  g4 |
  g4. g8 g4 g |
  fis e dis |
  d |
  fis e e d |
  d2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsXI
  \set stanza = #"1. "
  While shep -- herds watch’d their flocks by night,
    All seat -- ed on the ground,
  The an -- gel of the Lord came down,
    And glo -- ry shone a -- round,
    And glo -- ry shone a -- round.
}
altoWordsII = \lyricmode {
  \dropLyricsXI
  \set stanza = #"2. "
  “Fear not,” said he, for might -- y dread
    Had seized their troub -- led mind;
  “Glad tid -- ings of great joy I bring
    To you and all man -- kind,
    To you and all man -- kind.
}
altoWordsIII = \lyricmode {
  \dropLyricsXI
  \set stanza = #"3. "
  “To you, in Da -- vid’s town, this day,
    Is born of Da -- vid’s line,
  A Sav -- ior, Who is Christ the Lord;
    And this shall be the sign,
    And this shall be the sign:
}
altoWordsIV = \lyricmode {
  \dropLyricsXI
  \set stanza = #"4. "
  “The heav’n -- ly Babe you there shall find
    To hu -- man view dis -- play’d,
  All mean -- ly wrapp’d in swad -- dling clothes,
    And in a man -- ger laid,
    And in a man -- ger laid.”
}

altoWordsV = \lyricmode {
  \dropLyricsXI
  \set stanza = #"5. "
  Thus spake the ser -- aph, and forth -- with
    Ap -- peared a shin -- ing throng
  Of an -- gels prais -- ing God, Who thus
    Ad -- dressed their joy -- ful song,
    Ad -- dressed their joy -- ful song:
}

altoWordsVI = \lyricmode {
  \dropLyricsXI
  \set stanza = #"6. "
  “All glo -- ry be to God on high
    And to the earth be peace;
  Good -- will hence -- forth from heav’n to men,
    Be -- gin and nev -- er cease,
    Be -- gin and nev -- er cease.”
}

tenorMusic = \relative c' {
  b4 |
  d4. d8 d4 |
  b |
  c e d |
  d |
  d d e e |
  d2. |
  
  d4 |
  c4. b8 c4 d |
  d b b |
  b |
  d b c a |
  b2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'4 |
  g4. g8 d4 e |
  c c g' |
  d |
  g fis a a |
  d,2. |
  
  g4 |
  c4. g8 e4 g |
  d e b |
  g' |
  d e c d |
  g2. \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
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
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.9)) } \lyricsto "sopranos" \altoWords
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
    %#(layout-set-staff-size 15)
    #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20)))
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
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"While Shepherds Watched Their Flocks"}}
    poet = \markup\oldStyleNum"Nahum Tate (1652–1715)"
    composer = \markup\concat{\italic"Winchester Old" \oldStyleNum", by George Kirbye (c. 1565–1634)"}
  }
}

