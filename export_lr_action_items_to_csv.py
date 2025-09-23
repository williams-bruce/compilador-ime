import ast
import csv
from pathlib import Path


def extract_dict_literal_text(parsetab_path: Path, var_name: str) -> str:
    text = parsetab_path.read_text(encoding="utf-8")
    anchor = f"{var_name} ="
    start_idx = text.find(anchor)
    if start_idx == -1:
        raise RuntimeError(f"{var_name} assignment not found in parsetab.py")

    # Find the first '{' after the assignment
    brace_start = text.find("{", start_idx)
    if brace_start == -1:
        raise RuntimeError(f"Opening '{{' for {var_name} not found")

    # Scan forward to find the matching closing '}' using brace balance
    balance = 0
    end_idx = -1
    for i in range(brace_start, len(text)):
        ch = text[i]
        if ch == '{':
            balance += 1
        elif ch == '}':
            balance -= 1
            if balance == 0:
                end_idx = i
                break
    if end_idx == -1:
        raise RuntimeError(f"Closing '}}' for {var_name} not found")

    dict_literal = text[brace_start : end_idx + 1]
    return dict_literal


def load_items_dict(parsetab_path: Path, var_name: str) -> dict:
    dict_text = extract_dict_literal_text(parsetab_path, var_name)
    try:
        data = ast.literal_eval(dict_text)
    except Exception as exc:
        raise RuntimeError(f"Failed to parse {var_name} literal") from exc
    if not isinstance(data, dict):
        raise RuntimeError(f"Parsed {var_name} is not a dict")
    return data


def build_row_map(items: dict) -> tuple[list[str], dict[int, dict[str, int]], int]:
    # Preserve insertion order of keys as they appear in the literal
    tokens: list[str] = list(items.keys())

    row_to_values: dict[int, dict[str, int]] = {}
    max_row_index = -1

    for token in tokens:
        indices, values = items[token]
        if len(indices) != len(values):
            raise RuntimeError(f"Mismatched lengths for key {token}: {len(indices)} vs {len(values)}")
        for idx, val in zip(indices, values):
            if not isinstance(idx, int):
                raise RuntimeError(f"Non-integer row index for key {token}: {idx!r}")
            row_map = row_to_values.setdefault(idx, {})
            row_map[token] = val
            if idx > max_row_index:
                max_row_index = idx

    return tokens, row_to_values, max_row_index


def write_csv(tokens: list[str], rows: list[list[str]], out_path: Path) -> None:
    with out_path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f)
        # Header: tokens occupy the first row
        writer.writerow(tokens)
        # Data rows: row i corresponds to spreadsheet row (i + 2)
        writer.writerows(rows)


def main() -> None:
    parsetab_path = Path("parsetab.py")
    if not parsetab_path.exists():
        raise SystemExit("parsetab.py not found in current directory")

    # Load both dictionaries
    goto_items = load_items_dict(parsetab_path, "_lr_goto_items")
    action_items = load_items_dict(parsetab_path, "_lr_action_items")

    # Build row maps
    goto_tokens, goto_map, goto_max = build_row_map(goto_items)
    action_tokens, action_map, action_max = build_row_map(action_items)

    # Columns: goto first, then action
    tokens = goto_tokens + action_tokens

    # Rows: from 0..max of both
    max_row_index = max(goto_max, action_max)
    rows: list[list[str]] = []
    for row_index in range(0, max_row_index + 1):
        row_values: list[str] = []
        # Fill goto columns
        gmap = goto_map.get(row_index, {})
        for t in goto_tokens:
            v = gmap.get(t)
            row_values.append("" if v is None else str(v))
        # Fill action columns
        amap = action_map.get(row_index, {})
        for t in action_tokens:
            v = amap.get(t)
            row_values.append("" if v is None else str(v))
        rows.append(row_values)

    out_path = Path("lr_goto_and_action_items.csv")
    write_csv(tokens, rows, out_path)
    print(
        f"Wrote {out_path} with {len(rows)} data rows and {len(tokens)} columns "
        f"({len(goto_tokens)} goto + {len(action_tokens)} action)."
    )


if __name__ == "__main__":
    main()


