#include <QuartzCore/CAMetalLayer.h>
#import <AppKit/AppKit.h>

id clwInitMetal(NSWindow *window) {
    [window.contentView setWantsLayer:YES];
    id metal_layer = [CAMetalLayer layer];
    [window.contentView setLayer:metal_layer];

    return metal_layer;
}
