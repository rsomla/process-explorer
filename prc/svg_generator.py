# svg_generator.py: Python mockup for generating SVG visualizations of processes

def create_defs():
    return '<defs><marker id="arrow" viewBox="0 0 10 10" refX="5" refY="5" markerWidth="6" markerHeight="6" orient="auto"><path d="M 0 0 L 10 5 L 0 10 z"></path></marker></defs>'

def create_circle(cx, cy, r, fill):
    return f'<circle cx="{cx}" cy="{cy}" r="{r}" fill="{fill}" />'

def create_line(x1, y1, x2, y2, **attrs):
    attr_str = ' '.join(f'{k}="{v}"' for k, v in attrs.items())
    return f'<line x1="{x1}" y1="{y1}" x2="{x2}" y2="{y2}" {attr_str}/>'

def create_text(x, y, text):
    return f'<text x="{x}" y="{y}">{text}</text>'

def create_rect(x, y, w, h, **attrs):
    attr_str = ' '.join(f'{k}="{v}"' for k, v in attrs.items())
    return f'<rect x="{x}" y="{y}" width="{w}" height="{h}" {attr_str}/>'

STRIP_WIDTH = 64
GAP = 4
CENTER_OFFSET = STRIP_WIDTH / 2

def get_tooltip(node):
    if 'data' in node:
        return f'Data: {node["data"]}'
    if 'bind' in node:
        if 'any' in node['bind'] and node['bind']['any']:
            return 'Bind: _ (any)'
        return f'Bind: {node["bind"].get("var", "")}'
    return ''

def get_lane_count(node, path, expanded_states):
    id_ = '-'.join(map(str, path)) or 'root'
    is_expanded = expanded_states.get(id_, id_ == 'root')

    if node['type'] == 'parallel':
        if is_expanded:
            return sum(get_lane_count(sub, path + [i], expanded_states) for i, sub in enumerate(node['sub']))
        else:
            return 1
    return 1

def draw_parallel(node, x, y, path, expanded_states, svg_parts):
    id_ = '-'.join(map(str, path)) or 'root'
    is_expanded = expanded_states.get(id_, id_ == 'root')

    expanded_lane_count = sum(get_lane_count(sub, path + [i], expanded_states) for i, sub in enumerate(node['sub']))
    expanded_width = expanded_lane_count * STRIP_WIDTH + (expanded_lane_count - 1) * GAP
    bar_width = expanded_width if is_expanded else STRIP_WIDTH

    strip = None
    current_height = 0

    if id_ != 'root':
        strip_x = x
        onclick = "onclick=\"toggleExpand('{}')\"".format(id_) if 'onclick' in globals() else ''  # Placeholder for JS
        title = f'title="Ext: {" ".join(node.get("ext", []))}"' if 'ext' in node else ''
        svg_parts.append(create_rect(strip_x, y, bar_width, 50, fill="#e0e0e0", stroke="#333", rx="5", **{onclick: '', title: ''}))
        current_height = 50

    if is_expanded:
        sub_x = x if id_ == 'root' else x
        sub_y = y + (GAP if id_ != 'root' else 0)
        max_sub_height = 0
        for i, sub in enumerate(node['sub']):
            sub_path = path + [i]
            sub_lane_count = get_lane_count(sub, sub_path, expanded_states)
            sub_width = sub_lane_count * STRIP_WIDTH + (sub_lane_count - 1) * GAP
            sub_height = draw_process(sub, sub_x, sub_y, sub_width, sub_path, expanded_states, svg_parts)
            max_sub_height = max(max_sub_height, sub_height)
            sub_x += sub_width + GAP
        current_height = max_sub_height + (GAP if id_ != 'root' else 0)
        if strip:
            # Update height of strip
            svg_parts[-1] = svg_parts[-1].replace(' height="50"', f' height="{current_height}"')  # Patch previous rect
    elif strip:
        current_height = 50
        agg_ports = get_agg_ports(node)  # Use io for agg
        draw_aggregated_arrows(agg_ports, x, y, bar_width, 50, svg_parts)

    return current_height

def draw_process(node, x, y, width, path, expanded_states, svg_parts):
    center_x = x + CENTER_OFFSET
    if node['type'] == 'done':
        return draw_done(center_x, y, svg_parts)
    elif node['type'] == 'choice':
        return draw_choice(node, x, y, width, path, expanded_states, svg_parts)
    elif node['type'] == 'parallel':
        return draw_parallel(node, x, y, path, expanded_states, svg_parts)
    elif node['type'] == 'proc':
        return draw_proc(node, center_x, y, svg_parts)
    else:
        # Action (send/receive)
        action_y = y + 16
        svg_parts.append(create_circle(center_x, action_y, 5, "#333"))
        arrow_len = 20
        is_send = node['type'] == 'send'
        arrow_x = center_x + (arrow_len if is_send else -arrow_len)
        arrow = create_line(center_x, action_y, arrow_x, action_y, **{'marker-end': 'url(#arrow)', 'stroke': '#007bff' if is_send else '#ff7b00', 'stroke-width': '1.5', 'title': get_tooltip(node)})
        svg_parts.append(arrow)
        svg_parts.append(create_text(center_x + 4, action_y + 14, node['port']))

        cont_height = 24
        bottom_y = action_y + cont_height
        if 'cont' in node:
            cont_path = path + ['cont']
            sub_height = draw_process(node['cont'], x, action_y + 40, width, cont_path, expanded_states, svg_parts)
            cont_height += sub_height
            bottom_y = action_y + cont_height

        line = create_line(center_x, y, center_x, bottom_y, stroke="#333", **{'stroke-width': '2'})
        svg_parts.append(line)

        return bottom_y - y

def draw_done(x, y, svg_parts):
    term_y = y + 10
    svg_parts.append(create_line(x - 5, term_y, x + 5, term_y, stroke="#333", **{'stroke-width': '2'}))
    return 20

def draw_proc(node, x, y, svg_parts):
    svg_parts.append(create_rect(x - 30, y, 60, 30, fill="#fff", stroke="#666", title=f"Args: {node.get('args', []) }"))
    svg_parts.append(create_text(x - 25, y + 20, f"{node['name']}(...)"))
    return 30

def draw_choice(node, x, y, width, path, expanded_states, svg_parts):
    id_ = '-'.join(map(str, path)) or 'root'
    current_branch = choice_states.get(id_, 0)
    branch = node['branches'][current_branch]
    return draw_process(branch, x, y, width, path + [current_branch], expanded_states, svg_parts)

def get_agg_ports(node):
    receives = set(node.get('io', {}).get('in', []))
    sends = set(node.get('io', {}).get('out', []))
    return {'receives': list(receives), 'sends': list(sends)}

def draw_aggregated_arrows(agg_ports, bar_x, y, bar_width, height, svg_parts):
    is_collapsed = bar_width == STRIP_WIDTH
    receive_length = len(agg_ports['receives'])
    send_length = len(agg_ports['sends'])

    # Receives: inwards from left edge if collapsed
    for i, port in enumerate(agg_ports['receives']):
        ay = y + 16 + i * 15 - (receive_length - 1) * 7.5
        if is_collapsed:
            svg_parts.append(create_circle(bar_x, ay, 5, "#333"))
            arrow = create_line(bar_x, ay, bar_x + 20, ay, **{'marker-end': 'url(#arrow)', 'stroke': '#ff7b00', 'stroke-width': '1.5'})
            svg_parts.append(arrow)
            svg_parts.append(create_text(bar_x + 4, ay + 14, port))
        else:
            arrow = create_line(bar_x - 15, ay, bar_x, ay, **{'marker-end': 'url(#arrow)', 'stroke': '#ff7b00', 'stroke-width': '1.5'})
            svg_parts.append(arrow)
            svg_parts.append(create_text(bar_x - 35, ay + 5, port))

    # Sends: outwards from right edge if collapsed
    for i, port in enumerate(agg_ports['sends']):
        ay = y + 16 + i * 15 - (send_length - 1) * 7.5
        if is_collapsed:
            svg_parts.append(create_circle(bar_x + bar_width - 20, ay, 5, "#333"))
            arrow = create_line(bar_x + bar_width - 20, ay, bar_x + bar_width, ay, **{'marker-end': 'url(#arrow)', 'stroke': '#007bff', 'stroke-width': '1.5'})
            svg_parts.append(arrow)
            svg_parts.append(create_text(bar_x + bar_width - 60, ay + 14, port))
        else:
            arrow = create_line(bar_x + bar_width, ay, bar_x + bar_width + 15, ay, **{'marker-end': 'url(#arrow)', 'stroke': '#007bff', 'stroke-width': '1.5'})
            svg_parts.append(arrow)
            svg_parts.append(create_text(bar_x + bar_width + 20, ay + 5, port))

def draw_svg(process_data, expanded_states):
    svg_parts = []
    svg_parts.append(create_defs())
    draw_parallel(process_data, 4, 4, [], expanded_states, svg_parts)
    return '<svg id="process-svg" width="400" height="400" viewBox="0 0 400 400">' + ''.join(svg_parts) + '</svg>'

# Example usage
choice_states = {}  # Simulate

# Collapsed (default)
expanded_states = {}  # Empty for collapsed
collapsed_svg = draw_svg(processData, expanded_states)

# Expanded (simulate localStorage for id='1' as true)
expanded_states = {'1': True}
expanded_svg = draw_svg(processData, expanded_states)

print("Collapsed SVG:", collapsed_svg)
print("Expanded SVG:", expanded_svg)