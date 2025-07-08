# cl-raylib Examples

This directory contains Common Lisp implementations of raylib examples, organized to match the official raylib examples structure.

## Directory Structure

### core/ - Core functionality examples
- ✅ `core_basic_window.lisp` - Basic window creation
- ✅ `core_input_keys.lisp` - Keyboard input handling  
- ✅ `core_input_mouse.lisp` - Mouse input handling
- ✅ `core_input_mouse_wheel.lisp` - Mouse wheel input handling
- ✅ `core_random_values.lisp` - Random number generation
- ✅ `core_2d_camera.lisp` - 2D camera functionality
- ✅ `core_3d_camera_free.lisp` - Free 3D camera movement
- ✅ `core_world_screen.lisp` - World to screen coordinate conversion
- ✅ `core_window_should_close.lisp` - Custom window close handling
- ✅ `core_window_flags.lisp` - Window configuration flags

#### Missing core examples to implement:
- `core_2d_camera_mouse_zoom.lisp`
- `core_2d_camera_platformer.lisp` 
- `core_2d_camera_split_screen.lisp`
- `core_3d_camera_first_person.lisp`
- `core_3d_camera_mode.lisp`
- `core_3d_camera_split_screen.lisp`
- `core_3d_picking.lisp`
- `core_automation_events.lisp`
- `core_basic_screen_manager.lisp`
- `core_custom_frame_control.lisp`
- `core_custom_logging.lisp`
- `core_drop_files.lisp`
- `core_high_dpi.lisp`
- `core_input_gamepad.lisp`
- `core_input_gestures.lisp`
- `core_input_multitouch.lisp`
- `core_input_virtual_controls.lisp`
- `core_loading_thread.lisp`
- `core_random_sequence.lisp`
- `core_scissor_test.lisp`
- `core_smooth_pixelperfect.lisp`
- `core_storage_values.lisp`
- `core_vr_simulator.lisp`
- `core_window_letterbox.lisp`

### audio/ - Audio functionality examples
- ✅ `3d-audio-demo.lisp` - 3D positional audio
- ✅ `audio-demo.lisp` - Basic audio functionality  
- ✅ `audio-codec-demo.lisp` - Audio codec support

#### Missing audio examples to implement:
- `audio_mixed_processor.lisp`
- `audio_module_playing.lisp` 
- `audio_music_stream.lisp`
- `audio_raw_stream.lisp`
- `audio_sound_loading.lisp`
- `audio_sound_multi.lisp`
- `audio_sound_positioning.lisp`
- `audio_stream_effects.lisp`

### models/ - 3D model examples
- ✅ `3d-demo.lisp` - Basic 3D rendering
- ✅ `gltf-demo.lisp` - GLTF model loading
- ✅ `model-demo.lisp` - Model loading and rendering

#### Missing models examples to implement:
- `models_animation.lisp`
- `models_billboard.lisp`
- `models_bone_socket.lisp`
- `models_box_collisions.lisp`
- `models_cubicmap.lisp`
- `models_draw_cube_texture.lisp`
- `models_first_person_maze.lisp`
- `models_geometric_shapes.lisp`
- `models_gpu_skinning.lisp`
- `models_heightmap.lisp`
- `models_loading.lisp`
- `models_loading_gltf.lisp`
- `models_loading_m3d.lisp`
- `models_loading_vox.lisp`
- `models_mesh_generation.lisp`
- `models_mesh_picking.lisp`
- `models_orthographic_projection.lisp`
- `models_point_rendering.lisp`
- `models_rlgl_solar_system.lisp`
- `models_skybox.lisp`
- `models_tesseract_view.lisp`
- `models_waving_cubes.lisp`
- `models_yaw_pitch_roll.lisp`

### shapes/ - 2D shape drawing examples
- ✅ `collision-demo.lisp` - 2D collision detection
- ✅ `shapes_basic_shapes.lisp` - Basic 2D shape drawing
- ✅ `shapes_bouncing_ball.lisp` - Bouncing ball physics simulation

#### Missing shapes examples to implement:
- `shapes_collision_area.lisp`
- `shapes_colors_palette.lisp`
- `shapes_digital_clock.lisp`
- `shapes_draw_circle_sector.lisp`
- `shapes_draw_rectangle_rounded.lisp`
- `shapes_draw_ring.lisp`
- `shapes_easings_ball_anim.lisp`
- `shapes_easings_box_anim.lisp`
- `shapes_easings_rectangle_array.lisp`
- `shapes_following_eyes.lisp`
- `shapes_lines_bezier.lisp`
- `shapes_logo_raylib.lisp`
- `shapes_logo_raylib_anim.lisp`
- `shapes_rectangle_advanced.lisp`
- `shapes_rectangle_scaling.lisp`
- `shapes_splines_drawing.lisp`
- `shapes_top_down_lights.lisp`

### text/ - Text rendering examples
- ✅ `advanced-text-demo.lisp` - Advanced text features
- ✅ `font-loading-demo.lisp` - Font loading system
- ✅ `text-optimization-demo.lisp` - Text performance optimization
- ✅ `ttf-parsing-demo.lisp` - TTF font parsing
- ✅ `text_format_text.lisp` - Text formatting capabilities

#### Missing text examples to implement:
- `text_codepoints_loading.lisp`
- `text_draw_3d.lisp`
- `text_font_filters.lisp`
- `text_font_loading.lisp`
- `text_font_sdf.lisp`
- `text_font_spritefont.lisp`
- `text_input_box.lisp`
- `text_raylib_fonts.lisp`
- `text_rectangle_bounds.lisp`
- `text_unicode.lisp`
- `text_writing_anim.lisp`

### textures/ - Texture and image examples
- ✅ `image-processing-demo.lisp` - Image processing functionality
- ✅ `texture-demo.lisp` - Basic texture operations

#### Missing textures examples to implement:
- `textures_background_scrolling.lisp`
- `textures_blend_modes.lisp`
- `textures_bunnymark.lisp`
- `textures_draw_tiled.lisp`
- `textures_fog_of_war.lisp`
- `textures_gif_player.lisp`
- `textures_image_channel.lisp`
- `textures_image_drawing.lisp`
- `textures_image_generation.lisp`
- `textures_image_kernel.lisp`
- `textures_image_loading.lisp`
- `textures_image_processing.lisp`
- `textures_image_rotate.lisp`
- `textures_image_text.lisp`
- `textures_logo_raylib.lisp`
- `textures_mouse_painting.lisp`
- `textures_npatch_drawing.lisp`
- `textures_particles_blending.lisp`
- `textures_polygon.lisp`
- `textures_raw_data.lisp`
- `textures_sprite_anim.lisp`
- `textures_sprite_button.lisp`
- `textures_sprite_explosion.lisp`
- `textures_srcrec_dstrec.lisp`
- `textures_textured_curve.lisp`
- `textures_to_image.lisp`

### shaders/ - Shader examples
Currently empty - shader system not yet implemented.

### others/ - Miscellaneous examples
- ✅ `compression-demo.lisp` - Data compression
- ✅ `math-demo.lisp` - Mathematical functions
- ✅ `minimal-test.lisp` - Minimal functionality test

#### Missing others examples to implement:
- `easings_testbed.lisp`
- `embedded_files_loading.lisp`
- `raylib_opengl_interop.lisp`
- `raymath_vector_angle.lisp`
- `rlgl_compute_shader.lisp`
- `rlgl_standalone.lisp`

## Progress Summary

- **Total raylib examples**: ~180+
- **cl-raylib examples implemented**: 25
- **Completion percentage**: ~14%

## Priority Implementation Order

1. **Core examples** - Foundation functionality
2. **Shapes examples** - 2D graphics basics  
3. **Text examples** - Text rendering improvements
4. **Textures examples** - Image/texture handling
5. **Models examples** - 3D model features
6. **Audio examples** - Audio system expansion
7. **Shaders examples** - Advanced graphics
8. **Others examples** - Specialized features

## Notes

- All working examples follow the raylib naming convention
- Examples are organized to match the official raylib structure
- Each example includes proper documentation and comments
- Focus on core functionality first, then expand to advanced features