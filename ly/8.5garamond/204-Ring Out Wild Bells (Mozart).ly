\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Ring Out, Wild Bells"}}
  poet = \markup\oldStyleNum"Alfred Lord Tennyson (1809–1892)"
  composer = \markup\concat{"Adapted from " \italic "Kyrie" \oldStyleNum", 12th Mass"}
  arranger = \markup\oldStyleNum"W.A. Mozart (1756–1791)"
  tagline = \markup { "from" \italic {HymnWiki.org}}
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
  first-page-number = #204
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"new year"}
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
  \time 3/4
	\key g \major
	\override Rest.direction = #'0
	\override MultiMeasureRest.staff-position = #0
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}
sopWords = \lyricmode
{
  \dropLyricsV
	\set stanza = "1. "
	%\set vocalName = "Men/Women/Unison/SATB"
  Ring out, wild bells, \set ignoreMelismata = ##t to the \unset ignoreMelismata wild sky,
  The fly -- ing cloud, the frost -- y light:
  The year is dy -- ing in the night;
  Ring out, wild bells, and let him die.
        
	\set stanza = "5. "
  Ring out the want, the care, the sin,
  The faith -- less cold -- ness of the times:
  Ring out, ring out __ my mourn -- ful rhymes,
  But ring the full -- er min -- strel in.

}
sopWordsTwo = \lyricmode
{
  \dropLyricsV
	\set stanza = "2. "
  Ring out the old, ring in the new,
  Ring, hap -- py bells a -- cross the snow:
  The year is go -- ing, let him go;
  Ring out the false, ring in the true.
  
	\set stanza = "6. "
  Ring out false pride in place and blood,
  The civ -- ic slan -- der and the spite:
  Ring in the love __ of truth and right,
  Ring in the com -- mon love of good.
}
sopWordsThree = \lyricmode
{
  \dropLyricsV
	\set stanza = "3. "
  Ring out the grief that \set ignoreMelismata = ##t saps the \unset ignoreMelismata mind,
  For those that here we see no more:
  Ring out the feud __ of rich and poor,
  Ring in re -- dress to all man -- kind.
  
  
	\set stanza = "7. "
  Ring out old shapes of foul dis -- ease:
  Ring out the nar -- ’wing lust of gold:
  Ring out the thou -- sand wars of old,
  Ring in the thou -- sand years of peace.
}
sopWordsFour = \lyricmode
{
  \dropLyricsV
	\set stanza = "4. "
  Ring out a slow -- ly \set ignoreMelismata = ##t dy -- ing \unset ignoreMelismata cause,
  And an -- cient forms of par -- ty strife:
  Ring in the no -- bler modes of life,
  With sweet -- er man -- ners, pu -- rer laws.
        
  \set stanza = "8. "
  Ring in the val -- iant man and free,
  The lar -- ger heart, the kind -- lier hand;
  Ring out the dark -- ness of the land,
  Ring in the Christ that is to be.
}
sopMusic = {
  \repeat volta 2 {
    g'4 g' g' fis'2 d'4 c''2 c''4 b'2. \break d''4 d'' b'
    d''4 (c'') a' g'2 b'8[ a'] a'2. \break g'4 g' g' fis'4. (e'8) d'4
    c''2 c''4 b'2. \break d''4 d'' b' a' (c'') e'' g'2 fis'4 g'2.
    \break
  }
}

altoMusic = {
  \voiceTwo
  d'4 d' d' d'2 d'4 d'2 fis'4 g'2. g'4 g' g'
  e'2 d'4 d'2 g'8[ fis'] fis'2. g'4 g' g' fis'4. (e'8) d'4
  d'2 d'4 d'2. d'4 g'4 f'4 e'2 e'4 d'2 c'4 b2.
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
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
tenorMusic = {
  b4 b b a2 fis4 fis4 (a) d' d'2. b4 d' d'
  a2 c'4 b2 d'4 d'2. b4 d' b c'4. (b8) a8[ g]
  fis8 [g a b] c'8[ a] g2. b4 b d' c'2 a4 b2 a4 g2.
}
tenorWords = \lyricmode {

}

bassMusic = {
  g,4 b, g, d2 d4 d4 (fis) d g2. g4 b g
  a2 fis4 g2 g4 d2. g4 b g a4. (g8) fis8[ e]
  d8[ e fis g] a8[ fis] g2. g4 g gis a2 c4 d2 d4 g,2.
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
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold2 \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold2 \altoMusic >> }
    >>
    \new Lyrics \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWordsFour
    \new Lyrics \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWordsThree
    \new Lyrics \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWordsTwo
    \new Lyrics \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold2 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold2 \bassMusic >> }
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

