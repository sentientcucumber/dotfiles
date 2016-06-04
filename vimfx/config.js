// Standard vim keys are all over the place on the Dvorak keyboard. While its
// not the worst thing in the world, I want to use the home row as much as
// possible.
const LEFT = 'd';
const RIGHT = 'n';
const DOWN = 'h';
const UP = 't';

// Location
vimfx.set('mode.normal.focus_location_bar', 'o');
vimfx.set('mode.normal.focus_search_bar', 'O');

// History
vimfx.set('mode.normal.history_back', 'e');
vimfx.set('mode.normal.history_forward', 'u');

// Scrolling
vimfx.set('mode.normal.scroll_up', UP);
vimfx.set('mode.normal.scroll_down', DOWN);
vimfx.set('mode.normal.scroll_left', LEFT);
vimfx.set('mode.normal.scroll_right', RIGHT);
vimfx.set('mode.normal.scroll_to_top', 'gg');
vimfx.set('mode.normal.scroll_to_bottom', 'G');

// Follow
vimfx.set('mode.normal.follow', 'f');
vimfx.set('mode.normal.follow_in_tab', 'F');
vimfx.set('mode.hints.exit', '<escape>');

// Tabs
vimfx.set('mode.normal.tab_select_previous', LEFT.toUpperCase());
vimfx.set('mode.normal.tab_select_next', RIGHT.toUpperCase());
vimfx.set('mode.normal.tab_close', 'x');
vimfx.set('mode.normal.tab_restore', 'X');

// Search
vimfx.set('mode.normal.find', '/');
vimfx.set('mode.normal.find_highlight_all', 'a/');
vimfx.set('mode.normal.find_next', 'l');
vimfx.set('mode.normal.find_previous', 'L');
vimfx.set('mode.find.exit', '<escape>	<enter>');

