\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"O Come, All Ye Faithful"}}
  poet = \markup\oldStyleNum"Translated by Frederick Oakley (1802–1880)"
  composer = \markup\oldStyleNum"John Francis Wade (1711–1786)"
  tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
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
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #013
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
  \key a \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-0.5 . 1.5)
}

sopMusic = \relative c'' {
  \tempo 4 = 100
  \partial 4 \teeny a4 | \normalsize
  \slurDashed
  a2 e4( a) |
  b2( e,) |
  cis'4( b) cis( d) |
  cis2( b4) \bar"" \teeny a | \normalsize
  a2 gis4 fis |
  
  gis( a)( b) cis |
  \slurSolid gis2( fis4.) e8 |
  e2. b'4\rest |
  \slurDashed e2 d4( cis) |
  \slurDashed d2( cis) |
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
  \partial 4 \teeny e4 | \normalsize
  \tieDashed\slurDashed
  e2 e4~ e |
  e2~ e |
  e4~ e e( fis) |
  e2~ e4 \teeny cis | \normalsize
  \slurSolid cis( dis) e dis |
  
  \slurDashed e( dis8[) cis]( b4) e |
  \slurSolid e2( dis4.) e8 |
  e2. s4 |
  \slurDashed e2 fis8[ gis]( a4) | \slurSolid
  a( \slurDashed gis)( a2) |
  e4 e fis fis |
  \slurSolid\tieSolid \partial 4*3 e2 e4 |
  
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
    O come, all ye faith -- ful, Joy -- ful and tri -- um -- phant, O come ye, O come _ ye to Beth -- _ le -- hem; Come and be -- hold Him, Born the King of an -- _ gels;
  }
  \set stanza = \markup\dynamic"mf  "
  \lyricmode {
    \unset ignoreMelismata
    O come, let us a -- dore Him,
  }
  \set stanza = \markup\dynamic" f "
  \lyricmode {
    O come, let us a -- dore Him,
  }
  \set stanza = \markup\dynamic"ff  "
  \lyricmode {
    \raiseLyrics
    O come, let us a -- dore Him, __ Christ, __ the Lord!
  }
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  _ God, of __ _ God, __ _
  Light __ _ of __ _ Light, _ ""
  Lo, He ab -- hors _ not the Vir -- _ gin’s womb;
  Ve -- ry __ _ God,
  Be -- got -- ten, not cre -- at -- _ ed:
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  _ Sing, choirs of an -- gels, Sing with ex -- ul -- ta -- tions, ""
  Sing, all ye cit -- i -- zens of heav’n _ a -- bove;
  Glo -- ry to God, __ _ Glo -- ry in the high -- _ est;
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  _ Yea, Lord, we greet Thee, Born this hap -- py morn -- ing, ""
  Je -- sus, to Thee __ _ _ be glo -- _ ry giv’n;
  Word of the Fa -- ther, Now in flesh ap -- pear -- _ ing;
}

tenorMusic = \relative c' {
  \partial 4 \teeny cis4 | \normalsize
  \slurDashed\tieDashed
  cis2 cis4~ cis |
  b2~ b |
  a4( b) a~ a |
  a2( gis4) \teeny a | \normalsize
  a2 b4 b |
  
  b( a)( e') cis |
  \slurSolid b2( a4.) gis8 |
  gis2. d4\rest |
  \slurDashed cis'2 d4( e) |
  e2( e2) |
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
  \partial 4 \teeny a4 | \normalsize
  \slurDashed\tieDashed a2 a4~ a |
  gis2~ gis |
  a4( gis) a( d,) |
  e2~ e4 \teeny fis | \normalsize
  fis2 e4 b |
  
  e( cis)( gis) a |
  \tieSolid b2~b4. e8 |
  e2. s4 |
  cis'2 b4( a) |
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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

