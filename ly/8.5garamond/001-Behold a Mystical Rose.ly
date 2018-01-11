\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Behold a Mystical Rose"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Serdeczna Matko)"}}
  poet = \markup\oldStyleNum"Richard Cross"
  %meter = \markup\oldStyleNum"Translated by Edith M. G. Reed (1885–1933)"
  composer = \markup\oldStyleNum"Old Breton Air"
  %arranger = \markup\oldStyleNum"Arranged by Edith M. G. Reed (1885–1933)"
  tagline = ""
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
  first-page-number = #1
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key f \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c' {
  \partial 8
  c8 |
  f4 f8 g f g |
  a4.~ a4 \bar"" f8 |
  \slurDashed g( a) bes a( g) a |
  f4.~ f4 \bar""\break c8 |
  f4 f8 g( f) g |

  a4.~ a4 \bar"" f8 |
  g( a) bes a[ g] a |
  f4.~ f4 \bar""\break a8 |
  a4 g8 a4 bes8 |
  c4.~ c4 \bar"" bes8 |

  \slurDashed a( bes) c c bes a |
  g4.~ g4 \bar""\break a8 |
  f4 f8 g( f) g |
  a4.~ a4 \bar"" f8 |
  g( a) bes a g a |
  f4.~ f4 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \slurDashed
  c8 |
  c4 c8 c c c |
  f4.~ f4 f8 |
  e( d) c \tieDashed c~ c c |
  \tieSolid f4.~ f4

  c8 |
  \tieDashed c4 c8 c~ c c |
  \tieSolid f4.~ f4 f8 |
  e( d) c c4 c8 |
  f4.~ f4

  c8 |
  c4 e8 f4 d8 |
  e4.~ e4 g8 |
  f( g) a a g f |
  e4.~ e4

  f8 |
  \tieDashed c4 c8 c~ c c |
  \tieSolid f4.~ f4 f8 |
  e( d) c c c c |
  f4.~ f4
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Be -- hold a Mys -- ti -- cal Rose __ _
  From thorn -- _ y stem __ _ hath sprung; __ _
  Of Da -- vid’s lin -- e͜age God chose __ _
  To give to the world _ his Son. __ _
  All hail, thou House of Gold, __ _
  Of whom an -- cient proph -- ets fore -- told; __ _
  Thy roy -- al prais -- es we sing; __ _
  Thy womb was the Court of our King. __ _
}
altoWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Be -- hold the Mo -- ther of God, __ _
  Yet low -- li -- est daugh -- ter of men; __ _
  At her as -- sent -- _ ing nod __ _
  God gave __ _ his Son __ _ to men. __ _
  All hail, thou Morn -- ing Star, __ _
  Who brought bless -- ed light from a -- far, __ _
  Dis -- pel the dark -- ness of night, __ _
  Il -- lu -- mine our path with thy Light. __ _
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  Be -- hold the Cause of our Joy, __ _
  Who bore _ the In -- fi -- nite Child, __ _
  To Sa -- tan’s king -- dom de -- stroy __ _
  And men to God rec -- _ on -- cile. __ _
  All hail, thou Gate of Heav’n __ _
  Through thee __ _ all grac -- es are giv’n: __ _
  Through thee Sal -- va -- _ tion came; __ _
  All praised _ and blest be thy name! __ _
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
  \slurDashed
  c'8 |
  a4 a8 g a g |
  a4.~ a4 a8 |
  g( f) g a( bes) c |
  a4.~ a4

  c8 |
  a4 a8 g( a) g |
  a4.~ a4 a8 |
  g( f) g a[ bes] c |
  a4.~ a4

  a8 |
  a4 bes8 a4 bes8 |
  g4.~ g4 c8 |
  c( e) e e e c |
  c4.~ c4

  c8 |
  a4 a8 g( a) g |
  a4.~ a4 a8 |
  g( f) g a bes c |
  a4.~ a4

}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \slurDashed
  c8 |
  f4 f8 e f e |
  d4.~ d4 f8 |
  c( d) e f( e) f |
  \tieSolid f4.~ f4

  c8 |
  f4 f8 e( f) e |
  d4.~ d4 f8 |
  c( d) e f[ e] f |
  \tieSolid f4.~ f4
  
  f8 |
  f4 c8 d4 bes8 |
  c4.~ c4 e8 |
  f( c) a a c f |
  c4.~ c4

  f8 |
  f4 f8 e( f) e |
  d4.~ d4 f8 |
  c( d) e f e f |
  \tieSolid f4.~ f4
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "sopranosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "sopranosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "sopranosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "sopranosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "sopranos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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
    \Lyrics
    \override LyricText #'font-size = #2
  }
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
}

