\version "2.14.2"
\include "util.ly"
\header {
  tagline = \markup \concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"On Jordan’s Bank"}}
    poet = \markup\concat{\italic"Jordanis oras prævia" \oldStyleNum", by Charles Coffin (1676–1749)"}
    meter = \markup\oldStyleNum"Translated by John Chandler (1806–1876)"
    composer = \markup\italic"Winchester New"
    arranger = \markup\concat{"Adapted from Chorale in " \italic"Musikalisches Hand-Buch" \oldStyleNum", 1690"}
    tagline = \markup \concat{ "from " \italic "The English Hymnal" \oldStyleNum", 1906"}
  }
\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 50))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 70))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -0.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #006
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCaps advent}
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
#(set-global-staff-size 14.5) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.5 20))) }
global = {
  \key bes \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  %\tempo 4 = 56
  \partial 4 f4 |
  bes f g g |
  f ees d d |
  ees d c f |
  
  f e f\fermata \bar"" f |
  bes c d bes |
  ees d c d |
  
  bes g f bes |
  bes a bes\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 |
  f d ees ees8[ d] |
  c4 a bes bes |
  bes bes c d |
  
  d c c f |
  f ees d g8[ f] |
  ees4 f f f |
  
  f ees f d |
  g f f \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  On Jor -- dan’s bank the Bap -- tist’s cry
  An -- noun -- ces that the Lord is nigh;
  Come, then, and heark -- en, for he brings
  Glad tid -- ings from the King of kings!
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  Then cleansed be ev -- ’ry soul from sin;
  Make straight the way for God with -- in;
  Pre -- pare we in our hearts a home,
  Where such a might -- y Guest may come.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  For Thou art our sal -- va -- tion, Lord,
  Our Ref -- uge and our great re -- ward.
  With -- out Thy grace our souls must fade
  And with -- er like a flow’r de -- cayed.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  Stretch forth Thine hand, to heal our sore,
  And make us rise and fall no more;
  Once more up -- on Thy peo -- ple shine,
  And fill the world with love di -- vine.
}
altoWordsV = \lyricmode {
  \dropLyricsV
  \set stanza = #"5. "
  All praise, e -- ter -- nal Son, to Thee,
  Whose ad -- vent doth Thy peo -- ple free,
  Whom with the Fa -- ther, we a -- dore,
  And Ho -- ly Ghost, for ev -- er -- more.
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  bes4 |
  bes bes bes ees, |
  f f f f |
  g f8[ g] a4 a |
  
  bes8[ a] g4 a a |
  bes g8[ a] bes4 bes |
  bes8[ a] bes4 a bes |
  
  bes bes bes bes |
  c c d \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  bes4 |
  d bes ees c8[ bes] |
  a4 f bes bes |
  g bes f d' |
  
  bes c f, f'8[ ees] |
  d4 c bes ees8[ d] |
  c4 d8[ ees] f4 bes, |
  
  d ees d g |
  ees f bes, \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
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
    \tempo 4 = 112
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
