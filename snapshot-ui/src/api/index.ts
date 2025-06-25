export async function getSnapshots(): Promise<any[]> {
    const res = await fetch('/snapshots')
    if (!res.ok) throw new Error('Request failed')
    return res.json()
  }
  