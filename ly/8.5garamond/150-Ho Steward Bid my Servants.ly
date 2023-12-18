\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Ho! Steward, Bid My Servants"}}
  poet = \markup\oldStyleNum"John Mason Neale (1818–1866)"
  composer = \markup\oldStyleNum"Ancient ecclesiastical pre-Reformation melody"
  arranger = \markup\oldStyleNum"Arranged by Charles Wood (1866–1926)"
  tagline = \markup \concat{ "from " \italic "The Cambridge Carol Book" \oldStyleNum", 1924"}
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
  first-page-number = #150
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
  \key bes \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 2 {
    \partial 4 g'4 |
    d g g a |
    bes2 bes4 c8[ bes] |
    a4 g f e |
    f2. \bar""\break g4 |
    
    d g g a |
    bes2 bes4 bes8\rest bes |
    d4. c8 bes4 c |
    d2. \bar""\break f4 |
    
    d4. c8 bes4 d |
    f2 ees4 d |
    c4. bes8 a4 bes |
    c2. \bar""\break bes8[ c] |
    
    d4 bes c a |
    bes g d'4\fermata g, |
    d g g f |
    \partial 2. g2. | \break
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  bes8[ c] |
  d4 d d f |
  f2 f4 f |
  f d c c |
  c2. d8[ c] |
  
  bes4 d d f |
  f2 f4 s8 g |
  bes4. a8 g4 g |
  fis2. f4 |
  
  f4. f8 f4 bes |
  bes( a) g g |
  g f ees f |
  f2. f4 |
  
  f g g f |
  d4. c8 d4 ees |
  d d ees8[ d] ees4 |
  d2.
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  ‘Ho! stew -- ard, bid my ser -- vants
  Go forth, and hith -- er call,
  For guests, my friends and neigh -- bors,
  To sup with me in hall;
  That, at this bless -- ed sea -- son,
  Which comes but once a year,
  We may, as folk
  \set associatedVoice = "sopranos"
  in old -- en days,
  Re -- joice, and make good cheer.’
  
  \unset associatedVoice
  \set stanza = #"4. "
  ‘Sire, shall I bid the mer -- chant,
  That hath up -- on the seas
  His fleets of ca -- ra -- vel -- las,
  And right great ar -- go -- sies?’
  ‘Nay, bid me not the mer -- chant,
  But go and fetch the clerk,
  That with the ban -- 
  \set associatedVoice = "sopranos"
  dog goes to rest,
  And ris -- eth with the lark.’
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  ‘Sire, shall I bid the no -- ble,
  That ban -- quets in his state,
  With pur -- ple and fine lin -- en,
  With gold and sil -- ver plate?’
  ‘Nay, bid me not the no -- ble,
  For he hath got e -- now;
  But bring me in 
  \set associatedVoice = "sopranos"
  the coun -- try man,
  That liv -- eth by the plow.’
  
  \unset associatedVoice
  \set stanza = #"5. "
  ‘And where -- fore must I turn me
  From no -- ble and from rich?
  And where -- fore seek the poor man,
  That dwells in lane and ditch?’
  ‘Man, lay to heart the rea -- son,
  Be -- cause the King of all,
  Though rich, grew poor, 
  \set associatedVoice = "sopranos"
  for mor -- tal sake.
  And born was in a stall.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  ‘Sire, shall I bid in Di -- vès,
  For it is ve -- ry plain,
  If ye give him a ban -- quet,
  He’ll ban -- quet you a -- gain?’
  ‘Nay, bid not hith -- er Di -- vès,
  For it shall ne’er be thus,
  But go a -- mong 
  \set associatedVoice = "sopranos"
  the al -- ley -- lanes,
  And fetch in La -- za -- rus.’
  
  \unset associatedVoice
  \set stanza = #"6. "
  ‘For these be they, good stew -- ard,
  Whom God doth chief -- ly choose,
  And these, His poor -- er breth -- ren,
  No man may dare re -- fuse.
  So, in this bleak De -- cem -- ber,
  Then make we best good cheer,
  When, for the sake 
  \set associatedVoice = "sopranos"
  of Babe Je -- su,
  The poor we wel -- come here.’
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  g8[ a] |
  bes4 bes bes c |
  d2 d4 d |
  c bes c g |
  a2. g4 |
  
  g bes bes c |
  d2 d4 s8 ees |
  f4. d8 ees4 c |
  a2. c4 |
  
  bes4. a8 bes4 bes |
  c2 bes4 bes |
  c d ees d |
  a2. g8[ a] |
  
  bes4. d8 c4. d8 |
  bes4. c8 a4 c |
  d8[ c] bes[ a] g[ f] g[ a] |
  b2.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g4 |
  g' g g f |
  bes,2 bes4 d |
  f bes, a c |
  f2. bes,8[ a] |
  
  g4 g g' f |
  bes,2 bes4 d8\rest ees |
  bes'4. bes8 ees,4 ees |
  d2. a4 |
  
  bes4. c8 d4 g |
  f2 g4. f8 |
  ees4 d c bes |
  f'2. f4 |
  
  bes4. g8 a4. f8 |
  g4 ees d\fermata c |
  bes g c c g2.
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
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold2\altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold2\tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold2\bassMusic >> }
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

