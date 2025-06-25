<template>
    <section style="padding: 24px; max-width: 960px;">
      <h1 style="font-size: 2rem; font-weight: 700; margin-bottom: 16px;">
        Snapshot List
      </h1>
  
      <!-- 列表 -->
      <ul v-if="snapshots.length">
        <li v-for="snap in snapshots" :key="snap.snapshotId" style="margin-bottom: 12px;">
          <!-- snapshotId / snapshotName -->
          <strong>{{ snap.snapshotId }}</strong>
          <span v-if="snap.metadata?.snapshotName" style="color:#666;">
            — {{ snap.metadata.snapshotName }}
          </span>
  
          <!-- relations -->
          <ul v-if="Object.keys(snap.relations).length" style="padding-left: 24px; margin-top: 6px;">
            <li
              v-for="[relName, rel] in Object.entries(snap.relations)"
              :key="relName"
            >
            {{ relName }} ({{ rel.riDomainSort }} → {{ rel.riRangeSort }})
              <span v-if="rel.riVariables?.length">
                — vars: {{ rel.riVariables.join(', ') }}
              </span>
            </li>
          </ul>
          <em v-else style="color:#888;">(no relations)</em>
        </li>
      </ul>
  
      <p v-else>Loading snapshots…</p>
    </section>
  </template>
  
  <script setup lang="ts">
  import { ref, onMounted } from 'vue'
  import { getSnapshots } from '@/api'
  
  // === 类型 ===
  interface Relation {
    riRelationName: string
    riDomainSort: string
    riRangeSort: string
    riVariables: string[]
    riQuery: string
    [key: string]: any                // 兜底
  }
  interface Snapshot {
    snapshotId: string
    metadata?: { snapshotName?: string }
    relations: Record<string, Relation>
    [key: string]: any
  }
  
  const snapshots = ref<Snapshot[]>([])
  
  onMounted(async () => {
    try {
      snapshots.value = await getSnapshots()
      console.log('Fetched snapshots:', snapshots.value)
    } catch (e) {
      console.error('fetch error', e)
    }
  })
  </script>
  