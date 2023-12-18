\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Manger Throne"}}
  poet = \markup\oldStyleNum"William Chatterson Dix (1837–1898)"
  composer = \markup\oldStyleNum"Charles Steggall (1826–1905)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
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
  first-page-number = #118
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
}

sopMusic = \relative c'' {
  \slurDashed
  \tieDashed
  \partial 4 d4 |
  a b a fis8~ fis |
  e4~ e8 fis8 d4 e |
  fis b8~ b a~ a d4 | \break
  
  cis2. a4 |
  d4 cis8 b a g fis[ e] |
  fis4 gis8~ gis a4 b8~ b | \break
  cis4. d8 e~ e a,4 |
  
  b4~ b8 cis8 a4 a |
  b fis a a8 a | \break
  b4 fis a a8~ a |
  b8~ b d8~ d e4. fis8 |
  \partial 2. d2. \bar "||" \pageBreak
  
  d8 a b4 a8 a fis4 |
  e4. fis8 d4 e |
  fis4 b8 b a a d4 | \break
  cis2. a4 |
  
  d8( d) cis[ b] a( g) fis( e) |
  fis8~ fis gis4 a b8~ b | \break
  cis4. d8 e8~ e a,8~ a |
  b4.*2/3 \teeny cis8~ \normalsize cis8 a4 a |
  
  b4 fis8~ fis a4. a8 | \break
  b4 fis a a8~ a |
  b~ b d8~ d e~ e fis4 |
  \partial 2. d2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \slurDashed
  \tieDashed
  fis4 |
  d d d d8~ d |
  d4~ d8 d8 d4 a |
  d d8( e) fis~ fis g4 |
  
  a2. a4 |
  d cis8 b a g fis[ e] |
  d[ cis] b8~ b cis4 e8~ e |
  e4 fis e8~ e cis4 |
  
  d4~ d8 d8 cis4 cis |
  d d e d8 d |
  d4 d e d8~ d |
  d8~ d g8~ g g4. g8
  fis2. |
  
  fis8 d d4 d8 d d4 |
  d4. d8 d4 a |
  d d8 e fis fis g4 |
  a2. a4 |
  
  d8~ d cis[ b] a( g) fis( e) |
  d( cis) b4 cis e8~ e |
  e4 fis e8~ e cis8~ cis |
  d4.*2/3 \teeny d8~ \normalsize d8 cis4 cis |
  
  d4 d8~ d e4 d8~ d |
  d4 d e d8~ d |
  d~ d g8~ g g~ g g4
  fis2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  \set associatedVoice = "sopranos"
  \set ignoreMelismata = ##t
  Like sil -- ver lamps in a 
  \unset ignoreMelismata
  dis -- tant shrine,
  \unset associatedVoice
  The stars are __ spark -- ling bright;
  The bells of the ci -- ty of __ God ring __ out,
  \set ignoreMelismata = ##t
  For the Son of Ma -- ry was born __ _ to -- night;
  \unset ignoreMelismata
  The gloom is past, and the morn at last
  \set ignoreMelismata = ##t
  Is __ _ com -- ing with __ _ or -- ient light.
  
  \unset ignoreMelismata
  \set stanza = #"2. "
  \set associatedVoice = "sopranos"
  Nev -- er fell me -- lo -- dies half so sweet As those which are fill -- ing the skies;
  \unset associatedVoice
  \set ignoreMelismata = ##t
  And nev -- er a __ _ pa -- lace shone __ _ half __ _ so fair
  As the man -- ger bed __ _ where our Sav -- \skip1 ior lies;
  No night in the year is __ _ half so dear
  As __ _ this __ _ which has end -- ed our sighs.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"4. "
  \set associatedVoice = "sopranos"
  The stars of heav’n still
  \set ignoreMelismata = ##t
  shine as at first
  \unset associatedVoice
  They gleamed on this won -- der -- ful night;
  \unset ignoreMelismata
  The bells of the ci -- ty of __ God peal __ out,
  \set ignoreMelismata = ##t
  And the An -- gels’ song _ still rings in the height;
  And love still turns where the God -- head burns,
  Hid in flesh __ _ from __ _ flesh -- ly sight.
  
  \unset ignoreMelismata
  \set stanza = #"3. "
  \set associatedVoice = "sopranos"
  Now a new Pow’r has come on the earth,
  \unset associatedVoice
  A match for the arm -- ies of Hell:
  A child is __ born
  \set ignoreMelismata = ##t
  who shall con -- quer the foe,
  And __ _ all the spi -- rits of __ _ wicked -- ness _ quell:
  For Ma -- ry’s __ _ Son is the Might -- y One
  Whom the pro -- phets of __ _ God __ _ fore -- tell.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
  \set associatedVoice = "sopranos"
  Faith sees no long -- er the
  sta -- _ ble floor,
  \unset associatedVoice
  The pave -- ment of sap -- phire is there;
  \unset ignoreMelismata
  The clear light of Heav -- en streams out
  \set ignoreMelismata = ##t
  to the world;
  And __ _ Angels of God _ are
  crowd -- ing the air;
  And Heav’n and earth, through the spot -- less Birth,
  Are at peace __ _ on this night so fair.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  \slurDashed
  \tieDashed
  a4 |
  fis g fis a8~ a |
  g4~ g8 a8 fis4 a |
  a b8( cis) d~ d d4 |
  
  e2. a,4 |
  d cis8 b a g fis[ e] |
  fis'4 e8~ e e4 b8~ b |
  a4 a a8~ a a4 |
  
  gis4~ gis8 gis8 a4 cis |
  b d cis d8 d |
  b4 d cis d8~ d |
  d8~ d d8~ d cis4. cis8
  d2. |
  
  a8 fis g4 fis8 fis a4 |
  g4. a8 fis4 a |
  a b8 cis d d d4 |
  e2. a,4 |
  
  d8~ d cis[ b] a( g) fis( e) |
  fis'8~ fis e4 e b8~ b |
  a4 a a8~ a a8~ a |
  gis4.*2/3 \teeny gis8~ \normalsize gis8 a4 cis |
  
  b d8~ d cis4 d8~ d |
  b4 d cis d8~ d |
  d~ d d8~ d cis~ cis cis4
  <d a>2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \slurDashed
  \tieDashed
  d4 |
  d d d d8~ d |
  d4~ d8 d8 d4 cis |
  d g8~ g fis~ fis b4 |
  
  a2. a4 |
  d cis8 b a g fis[ e] |
  d4 e8~ e a4 gis8~ gis |
  a4 fis cis8~ cis fis4 |
  
  e4~ e8 e8 a,4 a' |
  g b a fis8 fis |
  g4 b a fis8~ fis |
  g8~ g b8~ b a4. a8
  d,2. |
  
  d8 d d4 d8 d d4 |
  d4. d8 d4 cis |
  d g8 g fis fis b4 |
  a2. a4 |
  
  d8~ d cis[ b] a( g) fis( e) |
  d8~ d e4 a gis8~ gis |
  a4 fis cis8~ cis fis8~ fis |
  e4.*2/3 \teeny e8~ \normalsize e8 a,4 a' |
  
  g b8~ b a4 fis8~ fis |
  g4 b a fis8~ fis |
  g8~ g b8~ b a~ a a4
  d,2. \bar "|."
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
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

