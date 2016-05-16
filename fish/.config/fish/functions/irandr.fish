function irandr --description 'Interactive xrander configuration'
    set -l screens (xrandr -q | grep ' connected' | sed 's/^\([A-Za-z0-9\-]*\).*/\1/')
    set -l previous

    for screen in $screens
        if not read_confirm -p "Activate display $screen" -d 0
            continue
        end

        
        read rotation -l -p "echo 'Rotate? [(N)ormal/(L)eft/(R)ight/(I)nverted] '"
        set -l position

        switch (echo $rotation | tr '[:upper:]' '[:lower:]')
        case l left
            set rotation left
        case r right
            set rotation right
        case i inverted
            set rotation inverted
        case n normal ''
            set rotation normal
        end

        if test -n "$previous"
            read position -p "echo 'Position? [(L)eft of/(R)ight of/(A)bove/(B)ellow/(S)ame as] '"

            switch (echo $position | tr '[:upper:]' '[:lower:]')
            case a above
                set position "--above $previous"
            case s same same-as
                set position "--same-as $previous"
            case l left left-of
                set position "--left-of $previous"
            case r right right-of ''
                set position "--right-of $previous"
            end
        else 
            set position '--primary'
        end

        echo "xrandr --output $screen --auto --rotate $rotation $position" | source

        set -l previous $screen
    end

    set -l dead_screens (xrandr | grep 'disconnected .* (' | sed 's/^\([A-Za-z0-9\-]*\).*/\1/')

    for screen in $dead_screens
        echo Disable $screen
        xrandr --output $screen --off
    end
end
